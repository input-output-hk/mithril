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
        // We should create a transaction  including delete and insert but it's not possible
        // with the current implementation because the connection is shared.
        self.connection
            .apply(DeleteStakePoolQuery::by_epoch(epoch)?)
            .with_context(|| format!("delete stakes failure, epoch: {epoch}"))?;

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
