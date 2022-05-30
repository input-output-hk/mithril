#![allow(dead_code)]
/// ↑ only until we plug this into main code
use mithril_common::entities::{Beacon, CertificatePending};

use thiserror::Error;

use super::{store_adapter::AdapterError, StoreAdapter};

#[derive(Debug, Error)]
pub enum StoreError {
    #[error("physical adapter returned an error: {0}")]
    AdapterError(#[from] AdapterError),
}

type Adapter = Box<dyn StoreAdapter<Key = Beacon, Record = CertificatePending>>;

pub struct CertificatePendingStore {
    adapter: Adapter,
}

impl CertificatePendingStore {
    pub async fn new(adapter: Adapter) -> Self {
        Self { adapter }
    }

    pub async fn get_from_beacon(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<CertificatePending>, StoreError> {
        Ok(self.adapter.get_record(beacon)?)
    }

    pub async fn save(&mut self, certificate: CertificatePending) -> Result<(), StoreError> {
        Ok(self
            .adapter
            .store_record(&certificate.beacon, &certificate)?)
    }

    pub async fn get_list(&self, last_n: usize) -> Result<Vec<CertificatePending>, StoreError> {
        let vars = self.adapter.get_last_n_records(last_n)?;
        let result = vars.into_iter().map(|(_, y)| y).collect();

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::certificate_store::dumb_adapter::DumbStoreAdapter;
    use mithril_common::fake_data;

    async fn get_certificate_pending_store(size: u64) -> CertificatePendingStore {
        let mut adapter: DumbStoreAdapter<Beacon, CertificatePending> = DumbStoreAdapter::new();

        for ix in 0..size {
            let beacon = Beacon::new("testnet".to_string(), ix / 3, ix);
            let certificate_pending = CertificatePending::new(
                beacon.clone(),
                fake_data::protocol_parameters(),
                ix.to_string(),
                fake_data::signers_with_stakes(5),
            );
            adapter.store_record(&beacon, &certificate_pending).unwrap();
        }
        let store = CertificatePendingStore::new(Box::new(adapter)).await;

        store
    }

    #[tokio::test]
    async fn list_is_empty() {
        let store = get_certificate_pending_store(0).await;

        assert_eq!(0, store.get_list(100).await.unwrap().len());
    }

    #[tokio::test]
    async fn list_has_some_members() {
        let store = get_certificate_pending_store(1).await;

        assert_eq!(1, store.get_list(100).await.unwrap().len());
    }

    #[tokio::test]
    async fn get_certificate_pending_with_good_beacon() {
        let beacon = Beacon::new("testnet".to_string(), 0, 0);
        let store = get_certificate_pending_store(1).await;
        let result = store.get_from_beacon(&beacon).await.unwrap();
        assert!(result.is_some());
    }

    #[tokio::test]
    async fn get_certificate_pending_with_wrong_beacon() {
        let beacon = Beacon::new("testnet".to_string(), 0, 1);
        let store = get_certificate_pending_store(1).await;
        let result = store.get_from_beacon(&beacon).await.unwrap();
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn save_certificate_pending_once() {
        let mut store = get_certificate_pending_store(1).await;
        let beacon = Beacon::new("testnet".to_string(), 0, 1);
        let certificate_pending = CertificatePending::new(
            beacon,
            fake_data::protocol_parameters(),
            "0".to_string(),
            fake_data::signers_with_stakes(1),
        );

        assert!(store.save(certificate_pending).await.is_ok());
    }

    #[tokio::test]
    async fn update_certificate_pending() {
        let mut store = get_certificate_pending_store(1).await;
        let beacon = Beacon::new("testnet".to_string(), 0, 0);
        let mut certificate_pending = store.get_from_beacon(&beacon).await.unwrap().unwrap();

        assert_eq!("0".to_string(), certificate_pending.previous_hash);
        certificate_pending.previous_hash = "one".to_string();

        assert!(store.save(certificate_pending).await.is_ok());
        let certificate_pending = store.get_from_beacon(&beacon).await.unwrap().unwrap();

        assert_eq!("one".to_string(), certificate_pending.previous_hash);
    }
}
