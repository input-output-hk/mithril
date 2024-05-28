use std::sync::Arc;

use async_trait::async_trait;

use mithril_common::entities::{Epoch, ProtocolParameters};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};
use mithril_persistence::store::adapter::AdapterError;

use crate::database::query::{
    DeleteEpochSettingQuery, GetEpochSettingQuery, UpdateEpochSettingQuery,
};
use crate::ProtocolParametersStorer;

/// Service to deal with epoch settings (read & write).
pub struct EpochSettingStore {
    connection: Arc<SqliteConnection>,

    /// Number of epochs before previous records will be pruned at the next call to
    /// [save_protocol_parameters][EpochSettingStore::save_protocol_parameters].
    retention_limit: Option<u64>,
}

impl EpochSettingStore {
    /// Create a new EpochSetting service
    pub fn new(connection: Arc<SqliteConnection>, retention_limit: Option<u64>) -> Self {
        Self {
            connection,
            retention_limit,
        }
    }
}

#[async_trait]
impl ProtocolParametersStorer for EpochSettingStore {
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<Option<ProtocolParameters>> {
        let epoch_setting_record = self
            .connection
            .fetch_first(UpdateEpochSettingQuery::one(epoch, protocol_parameters))
            .map_err(|e| {
                AdapterError::GeneralError(e.context("persist protocol parameters failure"))
            })?
            .unwrap_or_else(|| panic!("No entity returned by the persister, epoch = {epoch:?}"));

        // Prune useless old epoch settings.
        if let Some(threshold) = self.retention_limit {
            let _ = self
                .connection
                .fetch(DeleteEpochSettingQuery::below_epoch_threshold(
                    epoch - threshold,
                ))
                .map_err(AdapterError::QueryError)?
                .count();
        }

        Ok(Some(epoch_setting_record.protocol_parameters))
    }

    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>> {
        let mut cursor = self
            .connection
            .fetch(GetEpochSettingQuery::by_epoch(epoch)?)
            .map_err(|e| AdapterError::GeneralError(e.context("Could not get epoch setting")))?;

        if let Some(epoch_setting_record) = cursor.next() {
            return Ok(Some(epoch_setting_record.protocol_parameters));
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[tokio::test]
    async fn save_protocol_parameters_prune_older_epoch_settings() {
        const EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD: u64 = 5;

        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();
        let store = EpochSettingStore::new(
            Arc::new(connection),
            Some(EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD),
        );

        store
            .save_protocol_parameters(
                Epoch(2) + EPOCH_SETTING_PRUNE_EPOCH_THRESHOLD,
                fake_data::protocol_parameters(),
            )
            .await
            .expect("saving protocol parameters should not fails");
        let epoch1_params = store.get_protocol_parameters(Epoch(1)).await.unwrap();
        let epoch2_params = store.get_protocol_parameters(Epoch(2)).await.unwrap();

        assert!(
            epoch1_params.is_none(),
            "Protocol parameters at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_params.is_some(),
            "Protocol parameters at epoch 2 should still exist",
        );
    }
}
