use std::sync::Arc;

use anyhow::Ok;
use async_trait::async_trait;

use crate::database::query::{
    DeleteProtocolInitializerQuery, InsertOrReplaceProtocolInitializerQuery,
};
use crate::database::record::ProtocolInitializerRecord;
use crate::{
    database::query::GetProtocolInitializerQuery, services::EpochPruningTask,
    store::ProtocolInitializerStorer,
};
use mithril_common::{crypto_helper::ProtocolInitializer, entities::Epoch, StdResult};
use mithril_persistence::sqlite::ConnectionExtensions;
use mithril_persistence::{sqlite::SqliteConnection /*store::adapter::StoreAdapter*/};

/// Implementation of the ProtocolInitializerStorer
pub struct ProtocolInitializerRepository {
    connection: Arc<SqliteConnection>,
    retention_limit: Option<u64>,
}

impl ProtocolInitializerRepository {
    /// Create a new ProtocolInitializerRepository.
    pub fn new(connection: Arc<SqliteConnection>, retention_limit: Option<u64>) -> Self {
        Self {
            connection,
            retention_limit,
        }
    }
}

#[async_trait]
impl EpochPruningTask for ProtocolInitializerRepository {
    fn pruned_data(&self) -> &'static str {
        "Protocol initializer"
    }

    async fn prune(&self, epoch: Epoch) -> StdResult<()> {
        if let Some(threshold) = self.retention_limit {
            self.connection
                .apply(DeleteProtocolInitializerQuery::below_epoch_threshold(
                    epoch - threshold,
                ))?;
        }
        Ok(())
    }
}

#[async_trait]
impl ProtocolInitializerStorer for ProtocolInitializerRepository {
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> StdResult<Option<ProtocolInitializer>> {
        let previous_protocol_initializer = self.get_protocol_initializer(epoch).await?;
        let record = ProtocolInitializerRecord {
            epoch,
            protocol_initializer: protocol_initializer.clone(),
            created_at: chrono::Utc::now(),
        };
        self.connection
            .apply(InsertOrReplaceProtocolInitializerQuery::one(record).unwrap())?;

        Ok(previous_protocol_initializer)
    }

    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<ProtocolInitializer>> {
        let record = self
            .connection
            .fetch_first(GetProtocolInitializerQuery::for_epoch(epoch))?;

        Ok(record.map(|record| record.protocol_initializer))
    }

    async fn get_last_protocol_initializer(
        &self,
        last: usize,
    ) -> StdResult<Vec<(Epoch, ProtocolInitializer)>> {
        let record: Vec<ProtocolInitializerRecord> = self
            .connection
            .fetch_collect(GetProtocolInitializerQuery::last_n(last))?;

        Ok(record
            .iter()
            .map(|record| (record.epoch, record.protocol_initializer.to_owned()))
            .collect())
    }
}
