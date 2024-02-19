use std::collections::HashMap;

use async_trait::async_trait;
use mithril_common::StdResult;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, ProtocolParameters};

/// Store and get [protocol parameters][ProtocolParameters] for given epoch.
#[async_trait]
pub trait ProtocolParametersStorer: Sync + Send {
    /// Save the given `ProtocolParameter` for the given [Epoch].
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<Option<ProtocolParameters>>;

    /// Get the saved `ProtocolParameter` for the given [Epoch] if any.
    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>>;
}
/// `ProtocolParameter` store.
pub struct ProtocolParametersStore {
    adapter: RwLock<Adapter>,
    retention_len: Option<usize>,
}

pub struct FakeProtocolParametersStorer {
    pub protocol_parameters: RwLock<HashMap<Epoch, ProtocolParameters>>,
}

impl FakeProtocolParametersStorer {
    #[cfg(test)]
    pub fn new(data: Vec<(Epoch, ProtocolParameters)>) -> Self {
        let protocol_parameters = RwLock::new(data.into_iter().collect());
        Self {
            protocol_parameters,
        }
    }
}

#[async_trait]
impl ProtocolParametersStorer for FakeProtocolParametersStorer {
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> StdResult<Option<ProtocolParameters>> {
        let mut protocol_paremeters = self.protocol_parameters.write().await;
        Ok(protocol_paremeters.insert(epoch, protocol_parameters))
    }

    async fn get_protocol_parameters(&self, epoch: Epoch) -> StdResult<Option<ProtocolParameters>> {
        let protocol_paremeters = self.protocol_parameters.read().await;
        Ok(protocol_paremeters.get(&epoch).cloned())
    }

    async fn is_store_empty(&self) -> StdResult<bool> {
        let protocol_paremeters = self.protocol_parameters.read().await;
        Ok(protocol_paremeters.is_empty())
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
        let store = FakeProtocolParametersStorer::new(vec![]);
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
        let store = FakeProtocolParametersStorer::new(vec![(epoch, protocol_parameters.clone())]);
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
        let store = FakeProtocolParametersStorer::new(vec![(epoch, protocol_parameters.clone())]);
        let protocol_parameters_stored = store.get_protocol_parameters(epoch).await.unwrap();

        assert_eq!(Some(protocol_parameters), protocol_parameters_stored);
    }

    #[tokio::test]
    async fn test_get_protocol_parameters_do_not_exist() {
        let protocol_parameters = fake_data::protocol_parameters();
        let epoch = Epoch(1);
        let store = FakeProtocolParametersStorer::new(vec![(epoch, protocol_parameters.clone())]);
        let protocol_parameters_stored = store.get_protocol_parameters(epoch + 1).await.unwrap();

        assert!(protocol_parameters_stored.is_none());
    }
    }
}
