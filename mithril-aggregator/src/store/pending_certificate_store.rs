use super::StoreError;

use mithril_common::entities::CertificatePending;
use mithril_common::store::adapter::StoreAdapter;

type Adapter = Box<dyn StoreAdapter<Key = String, Record = CertificatePending>>;

const KEY: &str = "certificate_pending";

pub struct CertificatePendingStore {
    adapter: Adapter,
}

impl CertificatePendingStore {
    pub fn new(adapter: Adapter) -> Self {
        Self { adapter }
    }

    pub async fn get(&self) -> Result<Option<CertificatePending>, StoreError> {
        Ok(self.adapter.get_record(&KEY.to_string()).await?)
    }

    pub async fn save(&mut self, certificate: CertificatePending) -> Result<(), StoreError> {
        Ok(self
            .adapter
            .store_record(&KEY.to_string(), &certificate)
            .await?)
    }

    pub async fn remove(&mut self) -> Result<Option<CertificatePending>, StoreError> {
        self.adapter
            .remove(&KEY.to_string())
            .await
            .map_err(StoreError::AdapterError)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use mithril_common::entities::Beacon;
    use mithril_common::fake_data;
    use mithril_common::store::adapter::DumbStoreAdapter;

    async fn get_certificate_pending_store(is_populated: bool) -> CertificatePendingStore {
        let mut adapter: DumbStoreAdapter<String, CertificatePending> = DumbStoreAdapter::new();

        if is_populated {
            let beacon = Beacon::new("testnet".to_string(), 0, 0);
            let certificate_pending = CertificatePending::new(
                beacon.clone(),
                fake_data::protocol_parameters(),
                "previous_hash".to_string(),
                fake_data::signers(5),
            );
            adapter
                .store_record(&KEY.to_string(), &certificate_pending)
                .await
                .unwrap();
        }
        CertificatePendingStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn get_certificate_pending_with_existing_certificate() {
        let store = get_certificate_pending_store(true).await;
        let result = store.get().await.unwrap();

        assert!(result.is_some());
    }

    #[tokio::test]
    async fn get_certificate_pending_with_no_existing_certificate() {
        let store = get_certificate_pending_store(false).await;
        let result = store.get().await.unwrap();

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn save_certificate_pending_once() {
        let mut store = get_certificate_pending_store(false).await;
        let beacon = Beacon::new("testnet".to_string(), 0, 1);
        let certificate_pending = CertificatePending::new(
            beacon,
            fake_data::protocol_parameters(),
            "previous_hash".to_string(),
            fake_data::signers(1),
        );

        assert!(store.save(certificate_pending).await.is_ok());
        assert!(store.get().await.unwrap().is_some());
    }

    #[tokio::test]
    async fn update_certificate_pending() {
        let mut store = get_certificate_pending_store(true).await;
        let mut certificate_pending = store.get().await.unwrap().unwrap();

        assert_eq!(
            "previous_hash".to_string(),
            certificate_pending.previous_hash
        );
        certificate_pending.previous_hash = "something".to_string();

        assert!(store.save(certificate_pending).await.is_ok());
        let certificate_pending = store.get().await.unwrap().unwrap();

        assert_eq!("something".to_string(), certificate_pending.previous_hash);
    }

    #[tokio::test]
    async fn remove_certificate_pending() {
        let mut store = get_certificate_pending_store(true).await;
        let beacon = Beacon::new("testnet".to_string(), 0, 0);
        let certificate_pending = store.remove().await.unwrap().unwrap();

        assert_eq!(beacon, certificate_pending.beacon);
        assert!(store.get().await.unwrap().is_none());
    }
}
