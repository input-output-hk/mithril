use std::collections::HashMap;

use async_trait::async_trait;
use mithril_common::StdResult;
use slog_scope::debug;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, ProtocolParameters};

use crate::entities::AggregatorEpochSettings;

/// Store and get [protocol parameters][ProtocolParameters] for given epoch.
#[async_trait]
pub trait EpochSettingsStorer: Sync + Send {
    /// Save the given `AggregatorEpochSettings` for the given [Epoch].
    async fn save_epoch_settings(
        &self,
        epoch: Epoch,
        epoch_settings: AggregatorEpochSettings,
    ) -> StdResult<Option<AggregatorEpochSettings>>;

    /// Get the saved `ProtocolParameter` for the given [Epoch] if any.
    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>>;

    /// Get the saved `AggregatorEpochSettings` for the given [Epoch] if any.
    async fn get_epoch_settings(&self, epoch: Epoch) -> StdResult<Option<AggregatorEpochSettings>>;

    /// Handle discrepancies at startup in the epoch settings store.
    /// In case an aggregator has been launched after some epochs of not being up or at initial startup,
    /// the discrepancies in the epoch settings store needs to be fixed.
    /// The epoch settings needs to be recorded for the working epoch and the next 2 epochs.
    async fn handle_discrepancies_at_startup(
        &self,
        current_epoch: Epoch,
        epoch_settings_configuration: &AggregatorEpochSettings,
    ) -> StdResult<()> {
        for epoch_offset in 0..=2 {
            let epoch = current_epoch + epoch_offset;
            if self.get_epoch_settings(epoch).await?.is_none() {
                debug!("Handle discrepancies at startup of epoch settings store, will record epoch settings from the configuration for epoch {epoch}: {epoch_settings_configuration:?}");
                self.save_epoch_settings(epoch, epoch_settings_configuration.clone())
                    .await?;
            }
        }

        Ok(())
    }
}

pub struct FakeEpochSettingsStorer {
    pub epoch_settings: RwLock<HashMap<Epoch, AggregatorEpochSettings>>,
}

impl FakeEpochSettingsStorer {
    #[cfg(test)]
    pub fn new(data: Vec<(Epoch, AggregatorEpochSettings)>) -> Self {
        let epoch_settings = RwLock::new(data.into_iter().collect());
        Self { epoch_settings }
    }
}

#[async_trait]
impl EpochSettingsStorer for FakeEpochSettingsStorer {
    async fn save_epoch_settings(
        &self,
        epoch: Epoch,
        epoch_settings: AggregatorEpochSettings,
    ) -> StdResult<Option<AggregatorEpochSettings>> {
        let mut epoch_settings_write = self.epoch_settings.write().await;

        Ok(epoch_settings_write.insert(epoch, epoch_settings))
    }

    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>> {
        Ok(self
            .get_epoch_settings(epoch)
            .await?
            .map(|epoch_settings| epoch_settings.protocol_parameters.clone()))
    }

    async fn get_epoch_settings(&self, epoch: Epoch) -> StdResult<Option<AggregatorEpochSettings>> {
        let epoch_settings = self.epoch_settings.read().await;
        Ok(epoch_settings.get(&epoch).cloned())
    }
}

#[cfg(test)]
mod tests {

    use mithril_common::entities::CardanoTransactionsSigningConfig;

    use super::*;

    #[tokio::test]
    async fn test_save_epoch_settings_do_not_exist_yet_return_none() {
        let epoch_settings = AggregatorEpochSettings::dummy();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![]);
        let epoch_settings_previous = store
            .save_epoch_settings(epoch, epoch_settings)
            .await
            .unwrap();

        assert!(epoch_settings_previous.is_none());
    }

    #[tokio::test]
    async fn test_save_epoch_settings_already_exist_return_previous_epoch_settings_stored() {
        let epoch_settings = AggregatorEpochSettings::dummy();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![(epoch, epoch_settings.clone())]);
        let protocol_parameters_new = ProtocolParameters {
            k: epoch_settings.protocol_parameters.k + 1,
            ..epoch_settings.protocol_parameters
        };
        let epoch_settings_previous = store
            .save_epoch_settings(
                epoch,
                AggregatorEpochSettings {
                    protocol_parameters: protocol_parameters_new.clone(),
                    ..epoch_settings.clone()
                },
            )
            .await
            .unwrap();

        assert_eq!(Some(epoch_settings), epoch_settings_previous);
    }

    #[tokio::test]
    async fn test_get_epoch_settings_exist() {
        let epoch_settings = AggregatorEpochSettings::dummy();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![(epoch, epoch_settings.clone())]);
        let epoch_settings_stored = store.get_epoch_settings(epoch).await.unwrap();

        assert_eq!(Some(epoch_settings), epoch_settings_stored);
    }

    #[tokio::test]
    async fn test_get_epoch_settings_do_not_exist() {
        let epoch_settings = AggregatorEpochSettings::dummy();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![(epoch, epoch_settings.clone())]);
        let epoch_settings_stored = store.get_epoch_settings(epoch + 1).await.unwrap();

        assert!(epoch_settings_stored.is_none());
    }

    #[tokio::test]
    async fn test_handle_discrepancies_at_startup_should_complete_at_least_two_epochs() {
        let epoch_settings = AggregatorEpochSettings::dummy();
        let epoch_settings_new = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters {
                k: epoch_settings.protocol_parameters.k + 1,
                ..epoch_settings.protocol_parameters
            },
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig {
                step: epoch_settings.cardano_transactions_signing_config.step + 1,
                ..epoch_settings.cardano_transactions_signing_config
            },
        };
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![
            (epoch, epoch_settings.clone()),
            (epoch + 1, epoch_settings.clone()),
        ]);

        store
            .handle_discrepancies_at_startup(epoch, &epoch_settings_new)
            .await
            .unwrap();

        let epoch_settings_stored = store.get_epoch_settings(epoch).await.unwrap();
        assert_eq!(Some(epoch_settings.clone()), epoch_settings_stored);

        let epoch_settings_stored = store.get_epoch_settings(epoch + 1).await.unwrap();
        assert_eq!(Some(epoch_settings.clone()), epoch_settings_stored);

        let epoch_settings_stored = store.get_epoch_settings(epoch + 2).await.unwrap();
        assert_eq!(Some(epoch_settings_new.clone()), epoch_settings_stored);

        let epoch_settings_stored = store.get_epoch_settings(epoch + 3).await.unwrap();
        assert!(epoch_settings_stored.is_none());
    }
}
