use async_trait::async_trait;
use thiserror::Error;
use tokio::sync::RwLock;

use crate::entities::{Epoch, StakeDistribution};

use super::adapter::{AdapterError, StoreAdapter};

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = StakeDistribution>>;

/// [StakeStorer] related errors.
#[derive(Debug, Error)]
pub enum StakeStoreError {
    /// Error raised when the underlying [adapter][StoreAdapter] raise an error.
    #[error("adapter error {0}")]
    AdapterError(#[from] AdapterError),
}

/// Represent a way to store the stake of mithril party members.
#[async_trait]
pub trait StakeStorer {
    /// Save the stakes in the store for a given `epoch`.
    async fn save_stakes(
        &self,
        epoch: Epoch,
        stakes: StakeDistribution,
    ) -> Result<Option<StakeDistribution>, StakeStoreError>;

    /// Get the stakes of all party at a given `epoch`.
    async fn get_stakes(&self, epoch: Epoch) -> Result<Option<StakeDistribution>, StakeStoreError>;
}

/// A [StakeStorer] that use a [StoreAdapter] to store data.
pub struct StakeStore {
    adapter: RwLock<Adapter>,
}

impl StakeStore {
    /// StakeStore factory
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }
}

#[async_trait]
impl StakeStorer for StakeStore {
    async fn save_stakes(
        &self,
        epoch: Epoch,
        stakes: StakeDistribution,
    ) -> Result<Option<StakeDistribution>, StakeStoreError> {
        let mut adapter = self.adapter.write().await;
        let signers = adapter.get_record(&epoch).await?;
        adapter.store_record(&epoch, &stakes).await?;

        Ok(signers)
    }

    async fn get_stakes(&self, epoch: Epoch) -> Result<Option<StakeDistribution>, StakeStoreError> {
        Ok(self.adapter.read().await.get_record(&epoch).await?)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::super::adapter::MemoryAdapter;
    use super::*;

    fn init_store(nb_epoch: u64, signers_per_epoch: u64) -> StakeStore {
        let mut values: Vec<(Epoch, StakeDistribution)> = Vec::new();

        for epoch in 1..=nb_epoch {
            let mut signers: StakeDistribution = HashMap::new();

            for party_idx in 1..=signers_per_epoch {
                let party_id = format!("{}", party_idx);
                signers.insert(party_id.clone(), 100 * party_idx + 1);
            }
            values.push((epoch, signers));
        }

        let values = if values.len() > 0 { Some(values) } else { None };
        let adapter: MemoryAdapter<u64, StakeDistribution> = MemoryAdapter::new(values).unwrap();
        StakeStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let store = init_store(0, 0);
        let res = store
            .save_stakes(1, HashMap::from([("1".to_string(), 123)]))
            .await
            .expect("Test adapter should not fail.");

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_signer_in_store() {
        let store = init_store(1, 1);
        let res = store
            .save_stakes(1, HashMap::from([("1".to_string(), 123)]))
            .await
            .expect("Test adapter should not fail.");

        assert_eq!(
            HashMap::from([("1".to_string(), 101)]),
            res.expect("the result should not be empty"),
        );
    }

    #[tokio::test]
    async fn get_stakes_for_empty_epoch() {
        let store = init_store(2, 1);
        let res = store
            .get_stakes(0)
            .await
            .expect("Test adapter should not fail.");

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_stakes_for_existing_epoch() {
        let store = init_store(2, 2);
        let res = store
            .get_stakes(1)
            .await
            .expect("Test adapter should not fail.");

        assert!(res.is_some());
        assert_eq!(2, res.expect("Query result should not be empty.").len());
    }
}
