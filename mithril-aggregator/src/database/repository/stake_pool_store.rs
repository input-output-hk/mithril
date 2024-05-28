use std::ops::Not;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::entities::{Epoch, StakeDistribution};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};
use mithril_persistence::store::adapter::AdapterError;
use mithril_persistence::store::StakeStorer;

use crate::database::query::{
    DeleteStakePoolQuery, GetStakePoolQuery, InsertOrReplaceStakePoolQuery,
};
use crate::database::record::StakePool;

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
            .with_context(|| format!("persist stakes failure, epoch: {epoch}"))
            .map_err(AdapterError::GeneralError)?;

        // Prune useless old stake distributions.
        if let Some(threshold) = self.retention_limit {
            let _ = self
                .connection
                .fetch(DeleteStakePoolQuery::below_epoch_threshold(
                    epoch - threshold,
                ))
                .map_err(AdapterError::QueryError)?
                .count();
        }

        Ok(Some(StakeDistribution::from_iter(
            pools.into_iter().map(|p| (p.stake_pool_id, p.stake)),
        )))
    }

    async fn get_stakes(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>> {
        let cursor = self
            .connection
            .fetch(GetStakePoolQuery::by_epoch(epoch)?)
            .with_context(|| format!("get stakes failure, epoch: {epoch}"))
            .map_err(AdapterError::GeneralError)?;
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

#[cfg(test)]
mod tests {
    use crate::database::test_helper::{insert_stake_pool, main_db_connection};

    use super::*;

    #[tokio::test]
    async fn save_protocol_parameters_prune_older_epoch_settings() {
        let connection = main_db_connection().unwrap();
        const STAKE_POOL_PRUNE_EPOCH_THRESHOLD: u64 = 10;
        insert_stake_pool(&connection, &[1, 2]).unwrap();
        let store =
            StakePoolStore::new(Arc::new(connection), Some(STAKE_POOL_PRUNE_EPOCH_THRESHOLD));

        store
            .save_stakes(
                Epoch(2) + STAKE_POOL_PRUNE_EPOCH_THRESHOLD,
                StakeDistribution::from_iter([("pool1".to_string(), 100)]),
            )
            .await
            .expect("saving stakes should not fails");
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
}
