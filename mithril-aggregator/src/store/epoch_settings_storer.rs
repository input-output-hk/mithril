use std::collections::HashMap;

use async_trait::async_trait;
use mithril_common::StdResult;
use slog_scope::debug;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, ProtocolParameters};

/// Store and get [protocol parameters][ProtocolParameters] for given epoch.
#[async_trait]
pub trait EpochSettingsStorer: Sync + Send {
    /// Save the given `ProtocolParameter` for the given [Epoch].
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<Option<ProtocolParameters>>;

    /// Get the saved `ProtocolParameter` for the given [Epoch] if any.
    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>>;

    /// Handle discrepancies at startup in the protocol parameters store.
    /// In case an aggregator has been launched after some epochs of not being up or at initial startup,
    /// the discrepancies in the protocol parameters store needs to be fixed.
    /// The protocol parameters needs to be recorded for the working epoch and the next 2 epochs.
    async fn handle_discrepancies_at_startup(
        &self,
        current_epoch: Epoch,
        configuration_protocol_parameters: &ProtocolParameters,
    ) -> StdResult<()> {
        for epoch_offset in 0..=2 {
            let epoch = current_epoch + epoch_offset;
            if self.get_protocol_parameters(epoch).await?.is_none() {
                debug!("Handle discrepancies at startup of protocol parameters store, will record protocol parameters from the configuration for epoch {epoch}: {configuration_protocol_parameters:?}");
                self.save_protocol_parameters(epoch, configuration_protocol_parameters.clone())
                    .await?;
            }
        }

        Ok(())
    }
}

pub struct FakeEpochSettingsStorer {
    pub protocol_parameters: RwLock<HashMap<Epoch, ProtocolParameters>>,
}

impl FakeEpochSettingsStorer {
    #[cfg(test)]
    pub fn new(data: Vec<(Epoch, ProtocolParameters)>) -> Self {
        let protocol_parameters = RwLock::new(data.into_iter().collect());
        Self {
            protocol_parameters,
        }
    }
}

#[async_trait]
impl EpochSettingsStorer for FakeEpochSettingsStorer {
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<Option<ProtocolParameters>> {
        let mut protocol_parameters_write = self.protocol_parameters.write().await;
        Ok(protocol_parameters_write.insert(epoch, protocol_parameters))
    }

    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>> {
        let protocol_parameters = self.protocol_parameters.read().await;
        Ok(protocol_parameters.get(&epoch).cloned())
    }
}

#[cfg(test)]
mod tests {

    use mithril_common::test_utils::fake_data;

    use super::*;

    #[tokio::test]
    async fn test_save_protocol_parameters_do_not_exist_yet() {
        let protocol_parameters = fake_data::protocol_parameters();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![]);
        let protocol_parameters_previous = store
            .save_protocol_parameters(epoch, protocol_parameters)
            .await
            .unwrap();

        assert!(protocol_parameters_previous.is_none());
    }

    #[tokio::test]
    async fn test_save_protocol_parameters_already_exist() {
        let protocol_parameters = fake_data::protocol_parameters();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![(epoch, protocol_parameters.clone())]);
        let protocol_parameters_new = ProtocolParameters {
            k: protocol_parameters.k + 1,
            ..protocol_parameters
        };
        let protocol_parameters_previous = store
            .save_protocol_parameters(epoch, protocol_parameters_new)
            .await
            .unwrap();

        assert_eq!(Some(protocol_parameters), protocol_parameters_previous);
    }

    #[tokio::test]
    async fn test_get_protocol_parameters_exist() {
        let protocol_parameters = fake_data::protocol_parameters();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![(epoch, protocol_parameters.clone())]);
        let protocol_parameters_stored = store.get_protocol_parameters(epoch).await.unwrap();

        assert_eq!(Some(protocol_parameters), protocol_parameters_stored);
    }

    #[tokio::test]
    async fn test_get_protocol_parameters_do_not_exist() {
        let protocol_parameters = fake_data::protocol_parameters();
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![(epoch, protocol_parameters.clone())]);
        let protocol_parameters_stored = store.get_protocol_parameters(epoch + 1).await.unwrap();

        assert!(protocol_parameters_stored.is_none());
    }

    #[tokio::test]
    async fn test_handle_discrepancies_at_startup_should_complete_at_least_two_epochs() {
        let protocol_parameters = fake_data::protocol_parameters();
        let protocol_parameters_new = ProtocolParameters {
            k: protocol_parameters.k + 1,
            ..protocol_parameters
        };
        let epoch = Epoch(1);
        let store = FakeEpochSettingsStorer::new(vec![
            (epoch, protocol_parameters.clone()),
            (epoch + 1, protocol_parameters.clone()),
        ]);

        store
            .handle_discrepancies_at_startup(epoch, &protocol_parameters_new)
            .await
            .unwrap();

        let protocol_parameters_stored = store.get_protocol_parameters(epoch).await.unwrap();
        assert_eq!(
            Some(protocol_parameters.clone()),
            protocol_parameters_stored
        );

        let protocol_parameters_stored = store.get_protocol_parameters(epoch + 1).await.unwrap();
        assert_eq!(
            Some(protocol_parameters.clone()),
            protocol_parameters_stored
        );

        let protocol_parameters_stored = store.get_protocol_parameters(epoch + 2).await.unwrap();
        assert_eq!(
            Some(protocol_parameters_new.clone()),
            protocol_parameters_stored
        );

        let protocol_parameters_stored = store.get_protocol_parameters(epoch + 3).await.unwrap();
        assert!(protocol_parameters_stored.is_none());
    }
}
