use async_trait::async_trait;
use tokio::sync::RwLock;

use mithril_common::certificate_chain::{CertificateRetriever, CertificateRetrieverError};
use mithril_common::entities::{Beacon, Certificate};
use mithril_common::store::{adapter::StoreAdapter, StoreError};

type Adapter = Box<dyn StoreAdapter<Key = String, Record = Certificate>>;

/// Store for issued certificates.
pub struct CertificateStore {
    adapter: RwLock<Adapter>,
}

impl CertificateStore {
    /// Create a new instance.
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }

    /// Fetch a saved certificate from its hash signature.
    pub async fn get_from_hash(&self, hash: &str) -> Result<Option<Certificate>, StoreError> {
        let record = self
            .adapter
            .read()
            .await
            .get_record(&hash.to_string())
            .await?;

        Ok(record)
    }

    /// Fetch a saved certificate that was issued for the given beacon if any.
    pub async fn get_from_beacon(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<Certificate>, StoreError> {
        let adapter = self.adapter.read().await;
        let mut iterator = adapter.get_iter().await?;

        Ok(iterator.find(|cert| beacon == &cert.beacon))
    }

    /// Save the given certificate.
    pub async fn save(&self, certificate: Certificate) -> Result<(), StoreError> {
        self.adapter
            .write()
            .await
            .store_record(&certificate.hash, &certificate)
            .await?;

        Ok(())
    }

    /// Return the list of the `last_n` saved certificates sorted by creation
    /// time the most recent first.
    pub async fn get_list(&self, last_n: usize) -> Result<Vec<Certificate>, StoreError> {
        let vars = self.adapter.read().await.get_last_n_records(last_n).await?;
        let result = vars.into_iter().map(|(_, y)| y).collect();

        Ok(result)
    }
}

#[async_trait]
impl CertificateRetriever for CertificateStore {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        Ok(self
            .get_from_hash(certificate_hash)
            .await
            .map_err(|e| CertificateRetrieverError::General(e.to_string()))?
            .ok_or_else(|| CertificateRetrieverError::General("missing certificate".to_string()))?)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use mithril_common::store::adapter::MemoryAdapter;
    use mithril_common::test_utils::fake_data::{self};

    async fn get_certificate_store(size: u64) -> CertificateStore {
        let mut beacon = Beacon::new("devnet".to_string(), 1, 1);
        let mut certificates = vec![];

        for ix in 0..size {
            let mut certificate = fake_data::certificate(format!("cert_{:0>2}", ix));
            // Change the beacon to properly test [CertificateStore::get_from_beacon]
            certificate.beacon = beacon.clone();
            beacon.immutable_file_number += 1;

            certificates.push((certificate.hash.clone(), certificate));
        }

        let adapter: MemoryAdapter<String, Certificate> =
            MemoryAdapter::new(Some(certificates)).unwrap();
        CertificateStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn list_is_empty() {
        let store = get_certificate_store(0).await;

        assert_eq!(0, store.get_list(100).await.unwrap().len());
    }

    #[tokio::test]
    async fn list_has_some_members() {
        let store = get_certificate_store(1).await;

        assert_eq!(1, store.get_list(100).await.unwrap().len());
    }

    #[tokio::test]
    async fn get_certificate_with_good_hash() {
        let store = get_certificate_store(1).await;
        let result = store.get_from_hash("cert_00").await.unwrap();
        assert!(result.is_some());
    }

    #[tokio::test]
    async fn get_certificate_with_wrong_hash() {
        let store = get_certificate_store(1).await;
        let result = store.get_from_hash("cert_99").await.unwrap();
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn save_certificate_once() {
        let store = get_certificate_store(1).await;
        let certificate = fake_data::certificate("123".to_string());

        assert!(store.save(certificate).await.is_ok());
    }

    #[tokio::test]
    async fn update_certificate() {
        let store = get_certificate_store(1).await;
        let mut certificate = store.get_from_hash("cert_00").await.unwrap().unwrap();

        certificate.previous_hash = "whatever".to_string();
        assert!(store.save(certificate).await.is_ok());
        let certificate = store.get_from_hash("cert_00").await.unwrap().unwrap();

        assert_eq!("whatever".to_string(), certificate.previous_hash);
    }

    #[tokio::test]
    async fn get_from_beacon() {
        let store = get_certificate_store(12).await;

        let beacon = Beacon::new("devnet".to_string(), 1, 9);
        let certificate = store
            .get_from_beacon(&beacon)
            .await
            .unwrap()
            .expect("a certificate should have been found");
        assert_eq!(beacon, certificate.beacon);
    }

    #[tokio::test]
    async fn get_from_beacon_not_exist() {
        let store = get_certificate_store(4).await;

        let beacon = Beacon::new("devnet".to_string(), 1, 9);
        assert_eq!(None, store.get_from_beacon(&beacon).await.unwrap());
    }
}
