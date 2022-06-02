use async_trait::async_trait;
use std::collections::HashMap;
use thiserror::Error;

use super::adapter::{AdapterError, StoreAdapter};
use mithril_common::entities::Signer;

type Adapter = Box<dyn StoreAdapter<Key = u64, Record = HashMap<u64, Signer>>>;

#[derive(Debug, Error)]
pub enum VerificationKeyStoreError {
    #[error("adapter error {0}")]
    AdapterError(#[from] AdapterError),
}

#[async_trait]
trait VerificationKeyStoreTrait {
    async fn save_verification_key(
        &mut self,
        epoch: u64,
        key: Signer,
    ) -> Result<Option<Signer>, VerificationKeyStoreError>;

    async fn get_verification_keys(
        &self,
        epoch: u64,
    ) -> Result<Option<HashMap<u64, Signer>>, VerificationKeyStoreError>;
}
pub struct VerificationKeyStore {
    adapter: Adapter,
}

impl VerificationKeyStore {
    pub fn new(adapter: Adapter) -> Self {
        Self { adapter }
    }
}

#[async_trait]
impl VerificationKeyStoreTrait for VerificationKeyStore {
    async fn save_verification_key(
        &mut self,
        epoch: u64,
        signer: Signer,
    ) -> Result<Option<Signer>, VerificationKeyStoreError> {
        let mut signers = match self.adapter.get_record(&epoch).await? {
            Some(s) => s,
            None => HashMap::new(),
        };
        let prev_signer = signers.insert(signer.party_id, signer);
        let _ = self.adapter.store_record(&epoch, &signers).await?;

        Ok(prev_signer)
    }

    async fn get_verification_keys(
        &self,
        epoch: u64,
    ) -> Result<Option<HashMap<u64, Signer>>, VerificationKeyStoreError> {
        Ok(self.adapter.get_record(&epoch).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::super::MemoryAdapter;
    use super::*;

    fn init_store(nb_epoch: u64, signers_per_epoch: u64) -> VerificationKeyStore {
        let mut values: Vec<(u64, HashMap<u64, Signer>)> = Vec::new();

        for epoch in 1..=nb_epoch {
            let mut signers: HashMap<u64, Signer> = HashMap::new();

            for party_id in 1..=signers_per_epoch {
                let _ = signers.insert(
                    party_id,
                    Signer {
                        party_id: party_id,
                        verification_key: format!("vkey {}", party_id),
                    },
                );
            }
            values.push((epoch, signers));
        }

        let values = if values.len() > 0 { Some(values) } else { None };
        let adapter: MemoryAdapter<u64, HashMap<u64, Signer>> = MemoryAdapter::new(values).unwrap();
        let store = VerificationKeyStore::new(Box::new(adapter));

        store
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let mut store = init_store(0, 0);
        let res = store
            .save_verification_key(
                0,
                Signer {
                    party_id: 0,
                    verification_key: "OK".to_string(),
                },
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_signer_in_store() {
        let mut store = init_store(1, 1);
        let res = store
            .save_verification_key(
                1,
                Signer {
                    party_id: 1,
                    verification_key: "test".to_string(),
                },
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(
            Signer {
                party_id: 1,
                verification_key: "vkey 1".to_string(),
            },
            res.unwrap(),
        );
    }

    #[tokio::test]
    async fn get_verification_keys_for_empty_epoch() {
        let store = init_store(2, 1);
        let res = store.get_verification_keys(0).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_verification_keys_for_existing_epoch() {
        let store = init_store(2, 2);
        let res = store.get_verification_keys(1).await.unwrap();

        assert!(res.is_some());
        assert_eq!(2, res.unwrap().len());
    }
}
