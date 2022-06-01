#![allow(dead_code)]
/// ↑ only until we plug this into main code
use mithril_common::entities::Certificate;

use thiserror::Error;

use super::{AdapterError, StoreAdapter};

#[derive(Debug, Error)]
pub enum StoreError {
    #[error("physical adapter returned an error: {0}")]
    AdapterError(#[from] AdapterError),
}

type Adapter = Box<dyn StoreAdapter<Key = String, Record = Certificate>>;

pub struct CertificateStore {
    adapter: Adapter,
}

impl CertificateStore {
    pub fn new(adapter: Adapter) -> Self {
        Self { adapter }
    }

    pub async fn get_from_hash(&self, hash: &str) -> Result<Option<Certificate>, StoreError> {
        Ok(self.adapter.get_record(&hash.to_string()).await?)
    }

    pub async fn save(&mut self, certificate: Certificate) -> Result<(), StoreError> {
        Ok(self
            .adapter
            .store_record(&certificate.hash, &certificate)
            .await?)
    }

    pub async fn get_list(&self, last_n: usize) -> Result<Vec<Certificate>, StoreError> {
        let vars = self.adapter.get_last_n_records(last_n).await?;
        let result = vars.into_iter().map(|(_, y)| y).collect();

        Ok(result)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::store::adapter::DumbStoreAdapter;
    use mithril_common::fake_data::{self};

    async fn get_certificate_store(size: u64) -> CertificateStore {
        let mut adapter: DumbStoreAdapter<String, Certificate> = DumbStoreAdapter::new();

        for ix in 0..size {
            let certificate = fake_data::certificate(format!("cert_{:0>2}", ix));
            adapter
                .store_record(&certificate.hash, &certificate)
                .await
                .unwrap();
        }
        let store = CertificateStore::new(Box::new(adapter));

        store
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
        let mut store = get_certificate_store(1).await;
        let certificate = fake_data::certificate(format!("{}", "123".to_string()));

        assert!(store.save(certificate).await.is_ok());
    }

    #[tokio::test]
    async fn update_certificate() {
        let mut store = get_certificate_store(1).await;
        let mut certificate = store.get_from_hash("cert_00").await.unwrap().unwrap();

        certificate.previous_hash = "whatever".to_string();
        assert!(store.save(certificate).await.is_ok());
        let certificate = store.get_from_hash("cert_00").await.unwrap().unwrap();

        assert_eq!("whatever".to_string(), certificate.previous_hash);
    }
}
