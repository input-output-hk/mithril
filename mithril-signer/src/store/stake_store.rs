use async_trait::async_trait;
use mithril_common::entities::{Epoch, StakeDistribution};
use mithril_common::signable_builder::StakeDistributionRetriever;
use mithril_common::StdResult;
use mithril_persistence::store::StakeStorer;
use tokio::sync::RwLock;

use mithril_persistence::store::{adapter::StoreAdapter, StorePruner};

use crate::services::EpochPruningTask;

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = StakeDistribution>>;

/// A [StakeStorer] that uses a [StoreAdapter] to store data.
pub struct StakeStore {
    adapter: RwLock<Adapter>,
    retention_limit: Option<usize>,
}

impl StakeStore {
    /// StakeStore factory
    pub fn new(adapter: Adapter, retention_limit: Option<usize>) -> Self {
        Self {
            adapter: RwLock::new(adapter),
            retention_limit,
        }
    }
}

#[async_trait]
impl EpochPruningTask for StakeStore {
    fn pruned_data(&self) -> &'static str {
        "Stake"
    }

    async fn prune(&self, _epoch: Epoch) -> StdResult<()> {
        mithril_persistence::store::StorePruner::prune(self).await
    }
}

#[async_trait]
impl StorePruner for StakeStore {
    type Key = Epoch;
    type Record = StakeDistribution;

    fn get_adapter(
        &self,
    ) -> &RwLock<Box<dyn StoreAdapter<Key = Self::Key, Record = Self::Record>>> {
        &self.adapter
    }

    fn get_max_records(&self) -> Option<usize> {
        self.retention_limit
    }
}

#[async_trait]
impl StakeStorer for StakeStore {
    async fn save_stakes(
        &self,
        epoch: Epoch,
        stakes: StakeDistribution,
    ) -> StdResult<Option<StakeDistribution>> {
        let signers = {
            let mut adapter = self.adapter.write().await;
            let signers = adapter.get_record(&epoch).await?;
            adapter.store_record(&epoch, &stakes).await?;

            signers
        };

        Ok(signers)
    }

    async fn get_stakes(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>> {
        Ok(self.adapter.read().await.get_record(&epoch).await?)
    }
}

#[async_trait]
impl StakeDistributionRetriever for StakeStore {
    async fn retrieve(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>> {
        let stake_distribution = self.get_stakes(epoch).await?;

        Ok(stake_distribution)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_persistence::store::adapter::MemoryAdapter;

    fn init_store(
        nb_epoch: u64,
        signers_per_epoch: u64,
        retention_limit: Option<usize>,
    ) -> StakeStore {
        let mut values: Vec<(Epoch, StakeDistribution)> = Vec::new();

        for epoch in 1..=nb_epoch {
            let mut signers: StakeDistribution = StakeDistribution::new();

            for party_idx in 1..=signers_per_epoch {
                let party_id = format!("{party_idx}");
                signers.insert(party_id.clone(), 100 * party_idx + 1);
            }
            values.push((Epoch(epoch), signers));
        }

        let values = if !values.is_empty() {
            Some(values)
        } else {
            None
        };
        let adapter: MemoryAdapter<Epoch, StakeDistribution> = MemoryAdapter::new(values).unwrap();
        StakeStore::new(Box::new(adapter), retention_limit)
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let store = init_store(0, 0, None);
        let res = store
            .save_stakes(Epoch(1), StakeDistribution::from([("1".to_string(), 123)]))
            .await
            .expect("Test adapter should not fail.");

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_signer_in_store() {
        let store = init_store(1, 1, None);
        let res = store
            .save_stakes(Epoch(1), StakeDistribution::from([("1".to_string(), 123)]))
            .await
            .expect("Test adapter should not fail.");

        assert_eq!(
            StakeDistribution::from([("1".to_string(), 101)]),
            res.expect("the result should not be empty"),
        );
    }

    #[tokio::test]
    async fn get_stakes_for_empty_epoch() {
        let store = init_store(2, 1, None);
        let res = store
            .get_stakes(Epoch(0))
            .await
            .expect("Test adapter should not fail.");

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_stakes_for_existing_epoch() {
        let store = init_store(2, 2, None);
        let res = store
            .get_stakes(Epoch(1))
            .await
            .expect("Test adapter should not fail.");

        assert!(res.is_some());
        assert_eq!(2, res.expect("Query result should not be empty.").len());
    }

    #[tokio::test]
    async fn check_retention_limit() {
        let store = init_store(3, 2, Some(2));
        assert!(store.get_stakes(Epoch(1)).await.unwrap().is_some());

        // Whatever the epoch, it's the retention limit that matters.
        EpochPruningTask::prune(&store, Epoch(99)).await.unwrap();
        assert!(store.get_stakes(Epoch(1)).await.unwrap().is_none());
        assert!(store.get_stakes(Epoch(2)).await.unwrap().is_some());
        assert!(store.get_stakes(Epoch(3)).await.unwrap().is_some());
    }

    #[tokio::test]
    async fn retrieve_with_no_stakes_returns_none() {
        let store = init_store(0, 0, None);

        let result = store.retrieve(Epoch(1)).await.unwrap();

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn retrieve_returns_stake_distribution() {
        let stake_distribution_to_retrieve =
            StakeDistribution::from([("pool-123".to_string(), 123)]);
        let store = init_store(0, 0, None);
        store
            .save_stakes(Epoch(1), stake_distribution_to_retrieve.clone())
            .await
            .unwrap();

        let stake_distribution = store.retrieve(Epoch(1)).await.unwrap();

        assert_eq!(stake_distribution, Some(stake_distribution_to_retrieve));
    }
}
