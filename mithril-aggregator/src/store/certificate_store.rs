use tokio::sync::RwLock;

use mithril_common::entities::{Beacon, Certificate};
use mithril_common::store::{adapter::StoreAdapter, StoreError};

type Adapter = Box<dyn StoreAdapter<Key = String, Record = Certificate>>;

pub struct CertificateStore {
    adapter: RwLock<Adapter>,
}

impl CertificateStore {
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }

    pub async fn get_from_hash(&self, hash: &str) -> Result<Option<Certificate>, StoreError> {
        let record = self
            .adapter
            .read()
            .await
            .get_record(&hash.to_string())
            .await?;
        Ok(record)
    }

    pub async fn get_from_beacon(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<Certificate>, StoreError> {
        let adapter = self.adapter.read().await;
        let mut iterator = adapter.get_iter().await?;
        Ok(iterator.find(|cert| beacon == &cert.beacon))
    }

    pub async fn save(&self, certificate: Certificate) -> Result<(), StoreError> {
        self.adapter
            .write()
            .await
            .store_record(&certificate.hash, &certificate)
            .await?;
        Ok(())
    }

    pub async fn get_list(&self, last_n: usize) -> Result<Vec<Certificate>, StoreError> {
        let vars = self.adapter.read().await.get_last_n_records(last_n).await?;
        let result = vars.into_iter().map(|(_, y)| y).collect();

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use mithril_common::fake_data::{self};
    use mithril_common::store::adapter::MemoryAdapter;

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
