use std::sync::Arc;

use async_trait::async_trait;

use mithril_common::entities::{CardanoTransactionsSigningConfig, Epoch, ProtocolParameters};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};
use mithril_persistence::store::adapter::AdapterError;
use sqlite::Value;

use crate::database::query::{
    DeleteEpochSettingsQuery, GetEpochSettingsQuery, UpdateEpochSettingsQuery,
};
use crate::entities::AggregatorEpochSettings;
use crate::EpochSettingsStorer;

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

    #[deprecated(since = "0.5.72", note = "temporary fix, should be removed")]
    /// Replace empty JSON values '{}' injected with Migration #28
    pub fn replace_cardano_signing_config_empty_values(
        &self,
        cardano_signing_config: CardanoTransactionsSigningConfig,
    ) -> StdResult<()> {
        let query = r#"
            update epoch_setting 
            set cardano_transactions_signing_config = ?
            where cardano_transactions_signing_config == '{}'"#;

        let mut statement = self.connection.prepare(query)?;
        statement.bind::<&[(_, Value)]>(&[(
            1,
            serde_json::to_string(&cardano_signing_config)?.into(),
        )])?;

        statement.next()?;

        Ok(())
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
            .map_err(|e| AdapterError::GeneralError(e.context("persist epoch settings failure")))?
            .unwrap_or_else(|| panic!("No entity returned by the persister, epoch = {epoch:?}"));

        // Prune useless old epoch settings.
        if let Some(threshold) = self.retention_limit {
            let _ = self
                .connection
                .fetch(DeleteEpochSettingsQuery::below_epoch_threshold(
                    epoch - threshold,
                ))
                .map_err(AdapterError::QueryError)?
                .count();
        }

        Ok(Some(epoch_settings_record.into()))
    }

    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>> {
        Ok(self
            .get_epoch_settings(epoch)
            .await?
            .map(|epoch_settings| epoch_settings.protocol_parameters))
    }

    async fn get_epoch_settings(&self, epoch: Epoch) -> StdResult<Option<AggregatorEpochSettings>> {
        let mut cursor = self
            .connection
            .fetch(GetEpochSettingsQuery::by_epoch(epoch)?)
            .map_err(|e| AdapterError::GeneralError(e.context("Could not get epoch settings")))?;

        if let Some(epoch_settings_record) = cursor.next() {
            return Ok(Some(epoch_settings_record.into()));
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::BlockNumber;

    use crate::database::test_helper::{insert_epoch_settings, main_db_connection};

    use super::*;

    #[tokio::test]
    async fn replace_cardano_signing_config_empty_values_updates_only_empty_values() {
        let connection = main_db_connection().unwrap();
        connection.execute(
            r#"insert into epoch_setting (epoch_setting_id, protocol_parameters, cardano_transactions_signing_config) 
            values (
                1, 
                '{"k": 5, "m": 100, "phi_f": 0.65}', 
                '{"security_parameter": 70, "step": 20}'
            )"#,
        ).unwrap();
        connection.execute(
            r#"insert into epoch_setting (epoch_setting_id, protocol_parameters, cardano_transactions_signing_config) 
            values (
                2,
                '{"k": 73, "m": 100, "phi_f": 0.65}', 
                '{}'
            )"#,
        ).unwrap();

        let store = EpochSettingsStore::new(Arc::new(connection), None);

        let epoch_settings = store.get_epoch_settings(Epoch(1)).await.unwrap().unwrap();
        assert_eq!(
            CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(70),
                step: BlockNumber(20),
            },
            epoch_settings.cardano_transactions_signing_config
        );

        #[allow(deprecated)]
        store
            .replace_cardano_signing_config_empty_values(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(50),
                step: BlockNumber(10),
            })
            .unwrap();

        {
            let epoch_settings = store.get_epoch_settings(Epoch(1)).await.unwrap().unwrap();
            assert_eq!(
                CardanoTransactionsSigningConfig {
                    security_parameter: BlockNumber(70),
                    step: BlockNumber(20),
                },
                epoch_settings.cardano_transactions_signing_config
            );
        }
        {
            let epoch_settings = store.get_epoch_settings(Epoch(2)).await.unwrap().unwrap();
            assert_eq!(
                CardanoTransactionsSigningConfig {
                    security_parameter: BlockNumber(50),
                    step: BlockNumber(10),
                },
                epoch_settings.cardano_transactions_signing_config
            );
        }
    }

    #[tokio::test]
    async fn save_epoch_settings_prune_older_epoch_settings() {
        const EPOCH_SETTINGS_PRUNE_EPOCH_THRESHOLD: u64 = 5;

        let connection = main_db_connection().unwrap();
        insert_epoch_settings(&connection, &[1, 2]).unwrap();
        let store = EpochSettingsStore::new(
            Arc::new(connection),
            Some(EPOCH_SETTINGS_PRUNE_EPOCH_THRESHOLD),
        );

        store
            .save_epoch_settings(
                Epoch(2) + EPOCH_SETTINGS_PRUNE_EPOCH_THRESHOLD,
                AggregatorEpochSettings::dummy(),
            )
            .await
            .expect("saving epoch settings should not fails");
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
