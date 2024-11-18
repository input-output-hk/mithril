use std::ops::Not;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::entities::{Epoch, StakeDistribution};
use mithril_common::signable_builder::StakeDistributionRetriever;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};
use mithril_persistence::store::StakeStorer;

use crate::database::query::{
    DeleteStakePoolQuery, GetStakePoolQuery, InsertOrReplaceStakePoolQuery,
};
use crate::database::record::StakePool;
use crate::services::EpochPruningTask;

/// Service to deal with stake pools (read & write).
pub struct StakePoolStore {
    connection: Arc<SqliteConnection>,

    /// Number of epochs before previous records will be pruned at the next call to
    /// [save_protocol_parameters][StakePoolStore::save_stakes].
    retention_limit: Option<u64>,
}

impl StakePoolStore {
    /// Create a new StakePool service
    pub fn new(connection: Arc<SqliteConnection>, retention_limit: Option<u64>) -> Self {
        Self {
            connection,
            retention_limit,
        }
    }
}

#[async_trait]
impl StakeStorer for StakePoolStore {
    async fn save_stakes(
        &self,
        epoch: Epoch,
        stakes: StakeDistribution,
    ) -> StdResult<Option<StakeDistribution>> {
        let pools: Vec<StakePool> = self
            .connection
            .fetch_collect(InsertOrReplaceStakePoolQuery::many(
                stakes
                    .into_iter()
                    .map(|(pool_id, stake)| (pool_id, epoch, stake))
                    .collect(),
            ))
            .with_context(|| format!("persist stakes failure, epoch: {epoch}"))?;

        Ok(Some(StakeDistribution::from_iter(
            pools.into_iter().map(|p| (p.stake_pool_id, p.stake)),
        )))
    }

    async fn get_stakes(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>> {
        let cursor = self
            .connection
            .fetch(GetStakePoolQuery::by_epoch(epoch)?)
            .with_context(|| format!("get stakes failure, epoch: {epoch}"))?;
        let mut stake_distribution = StakeDistribution::new();

        for stake_pool in cursor {
            stake_distribution.insert(stake_pool.stake_pool_id, stake_pool.stake);
        }

        Ok(stake_distribution
            .is_empty()
            .not()
            .then_some(stake_distribution))
    }
}

#[async_trait]
impl StakeDistributionRetriever for StakePoolStore {
    async fn retrieve(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>> {
        self.get_stakes(epoch).await
    }
}

#[async_trait]
impl EpochPruningTask for StakePoolStore {
    fn pruned_data(&self) -> &'static str {
        "Stake pool"
    }

    async fn prune(&self, epoch: Epoch) -> StdResult<()> {
        if let Some(threshold) = self.retention_limit {
            self.connection
                .apply(DeleteStakePoolQuery::below_epoch_threshold(
                    epoch - threshold,
                ))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions};

    use super::*;
    use crate::database::test_helper::{insert_stake_pool, main_db_connection, FakeStoreAdapter};

    #[tokio::test]
    async fn prune_epoch_settings_older_than_threshold() {
        let connection = main_db_connection().unwrap();
        const STAKE_POOL_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        insert_stake_pool(&connection, &[1, 2]).unwrap();
        let store =
            StakePoolStore::new(Arc::new(connection), Some(STAKE_POOL_PRUNE_EPOCH_THRESHOLD));

        store
            .prune(Epoch(2) + STAKE_POOL_PRUNE_EPOCH_THRESHOLD)
            .await
            .unwrap();

        let epoch1_stakes = store.get_stakes(Epoch(1)).await.unwrap();
        let epoch2_stakes = store.get_stakes(Epoch(2)).await.unwrap();

        assert_eq!(
            None, epoch1_stakes,
            "Stakes at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_stakes.is_some(),
            "Stakes at epoch 2 should still exist",
        );
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let connection = main_db_connection().unwrap();
        insert_stake_pool(&connection, &[1, 2]).unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        store.prune(Epoch(100)).await.unwrap();

        let epoch1_stakes = store.get_stakes(Epoch(1)).await.unwrap();
        let epoch2_stakes = store.get_stakes(Epoch(2)).await.unwrap();

        assert!(
            epoch1_stakes.is_some(),
            "Stakes at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_stakes.is_some(),
            "Stakes at epoch 2 should still exist",
        );
    }

    #[tokio::test]
    async fn retrieve_with_no_stakes_returns_none() {
        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);

        let result = store.retrieve(Epoch(1)).await.unwrap();

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn retrieve_returns_stake_distribution() {
        let stake_distribution_to_retrieve =
            StakeDistribution::from([("pool-123".to_string(), 123)]);
        let connection = main_db_connection().unwrap();
        let store = StakePoolStore::new(Arc::new(connection), None);
        store
            .save_stakes(Epoch(1), stake_distribution_to_retrieve.clone())
            .await
            .unwrap();

        let stake_distribution = store.retrieve(Epoch(1)).await.unwrap();

        assert_eq!(stake_distribution, Some(stake_distribution_to_retrieve));
    }

    #[tokio::test]
    async fn should_migrate_data_from_adapter() {
        let migrations = crate::database::migration::get_migrations();

        // TODO: Do it in test_helper (it is done by build_main_db_connection)
        fn create_connection_builder() -> ConnectionBuilder {
            ConnectionBuilder::open_memory()
                .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        }
        let connection = Arc::new(create_connection_builder().build().unwrap());

        // The adapter will create the table.
        let stake_adapter = FakeStoreAdapter::new(connection.clone(), "stake");
        // The adapter will create the table.
        stake_adapter.create_table();

        assert!(connection.prepare("select * from stake;").is_ok());
        assert!(connection.prepare("select * from db_version;").is_err());
        assert!(connection.prepare("select * from stake_pool;").is_err());

        // Here we can add some data with the old schema.
        let stake_distribution_to_retrieve =
            StakeDistribution::from([("pool-123".to_string(), 123)]);

        // If we don't want to use the adapter anymore, we can execute request directly.
        assert!(!stake_adapter.is_key_hash_exist("HashEpoch5"));
        stake_adapter
            .store_record("HashEpoch5", &Epoch(5), &stake_distribution_to_retrieve)
            .unwrap();
        assert!(stake_adapter.is_key_hash_exist("HashEpoch5"));

        // We finish the migration
        create_connection_builder()
            .apply_migrations(&connection, migrations)
            .unwrap();
        assert!(connection.prepare("select * from stake;").is_err());
        assert!(connection.prepare("select * from stake_pool;").is_ok());

        // We can check that data are migrated.
        let store = StakePoolStore::new(connection, None);
        let stake_distribution = store.retrieve(Epoch(5)).await.unwrap();
        assert_eq!(stake_distribution, Some(stake_distribution_to_retrieve));
    }
}
