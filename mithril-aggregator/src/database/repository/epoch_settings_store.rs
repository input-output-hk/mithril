use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, ProtocolParameters};
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    DeleteEpochSettingsQuery, GetEpochSettingsQuery, UpdateEpochSettingsQuery,
};
use crate::entities::AggregatorEpochSettings;
use crate::services::EpochPruningTask;
use crate::{EpochSettingsStorer, ProtocolParametersRetriever};

/// Service to deal with epoch settings (read & write).
pub struct EpochSettingsStore {
    connection: Arc<SqliteConnection>,

    /// Number of epochs before previous records will be pruned at the next call to
    /// [save_protocol_parameters][EpochSettingStore::save_protocol_parameters].
    retention_limit: Option<u64>,
}

impl EpochSettingsStore {
    /// Create a new EpochSettings store
    pub fn new(connection: Arc<SqliteConnection>, retention_limit: Option<u64>) -> Self {
        Self {
            connection,
            retention_limit,
        }
    }
}

#[async_trait]
impl ProtocolParametersRetriever for EpochSettingsStore {
    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>> {
        Ok(self
            .get_epoch_settings(epoch)
            .await?
            .map(|epoch_settings| epoch_settings.protocol_parameters))
    }
}

#[async_trait]
impl EpochSettingsStorer for EpochSettingsStore {
    async fn save_epoch_settings(
        &self,
        epoch: Epoch,
        epoch_settings: AggregatorEpochSettings,
    ) -> StdResult<Option<AggregatorEpochSettings>> {
        let epoch_settings_record = self
            .connection
            .fetch_first(UpdateEpochSettingsQuery::one(epoch, epoch_settings))
            .with_context(|| format!("persist epoch settings failure for epoch {epoch:?}"))?
            .unwrap_or_else(|| panic!("No entity returned by the persister, epoch = {epoch:?}"));

        Ok(Some(epoch_settings_record.into()))
    }

    async fn get_epoch_settings(&self, epoch: Epoch) -> StdResult<Option<AggregatorEpochSettings>> {
        let mut cursor = self
            .connection
            .fetch(GetEpochSettingsQuery::by_epoch(epoch)?)
            .with_context(|| format!("Could not get epoch settings: epoch = {epoch:?}"))?;

        if let Some(epoch_settings_record) = cursor.next() {
            return Ok(Some(epoch_settings_record.into()));
        }
        Ok(None)
    }
}

#[async_trait]
impl EpochPruningTask for EpochSettingsStore {
    fn pruned_data(&self) -> &'static str {
        "Epoch settings"
    }

    /// Prune useless old epoch settings.
    async fn prune(&self, epoch: Epoch) -> StdResult<()> {
        if let Some(threshold) = self.retention_limit {
            self.connection
                .apply(DeleteEpochSettingsQuery::below_epoch_threshold(
                    epoch - threshold,
                ))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[tokio::test]
    async fn prune_epoch_settings_older_than_threshold() {
        const EPOCH_SETTINGS_PRUNE_EPOCH_THRESHOLD: u64 = 5;

        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();
        let store = EpochSettingsStore::new(
            Arc::new(connection),
            Some(EPOCH_SETTINGS_PRUNE_EPOCH_THRESHOLD),
        );

        store
            .prune(Epoch(2) + EPOCH_SETTINGS_PRUNE_EPOCH_THRESHOLD)
            .await
            .unwrap();

        let epoch1_params = store.get_epoch_settings(Epoch(1)).await.unwrap();
        let epoch2_params = store.get_epoch_settings(Epoch(2)).await.unwrap();

        assert!(
            epoch1_params.is_none(),
            "Epoch settings at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_params.is_some(),
            "Epoch settings at epoch 2 should still exist",
        );
    }

    #[tokio::test]
    async fn without_threshold_nothing_is_pruned() {
        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();
        let store = EpochSettingsStore::new(Arc::new(connection), None);

        store.prune(Epoch(100)).await.unwrap();

        let epoch1_params = store.get_epoch_settings(Epoch(1)).await.unwrap();
        let epoch2_params = store.get_epoch_settings(Epoch(2)).await.unwrap();

        assert!(
            epoch1_params.is_some(),
            "Epoch settings at epoch 1 should have been pruned",
        );
        assert!(
            epoch2_params.is_some(),
            "Epoch settings at epoch 2 should still exist",
        );
    }

    #[tokio::test]
    async fn save_epoch_settings_stores_in_database() {
        let connection = main_db_connection().unwrap();

        let store = EpochSettingsStore::new(Arc::new(connection), None);

        store
            .save_epoch_settings(Epoch(2), AggregatorEpochSettings::dummy())
            .await
            .expect("saving epoch settings should not fails");
        {
            let epoch_settings = store.get_epoch_settings(Epoch(1)).await.unwrap();
            assert_eq!(None, epoch_settings);
        }
        {
            let epoch_settings = store.get_epoch_settings(Epoch(2)).await.unwrap().unwrap();
            assert_eq!(AggregatorEpochSettings::dummy(), epoch_settings);
        }
        {
            let epoch_settings = store.get_epoch_settings(Epoch(3)).await.unwrap();
            assert_eq!(None, epoch_settings);
        }
    }
}
