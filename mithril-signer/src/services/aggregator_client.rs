use async_trait::async_trait;

use mithril_aggregator_client::AggregatorHttpClient;
use mithril_aggregator_client::query::GetEpochSettingsQuery;
use mithril_common::{StdResult, messages::TryFromMessageAdapter};

use crate::entities::RegisteredSigners;
use crate::message_adapters::FromEpochSettingsAdapter;

/// Trait for mocking and testing a `AggregatorClient`
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Retrieves epoch settings from the aggregator
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>>;
}

#[async_trait]
impl AggregatorClient for AggregatorHttpClient {
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
        let message = self.send(GetEpochSettingsQuery::current()).await?;
        let epoch_settings = FromEpochSettingsAdapter::try_adapt(message)?;

        Ok(Some(epoch_settings))
    }
}

#[cfg(test)]
pub(crate) mod dumb {
    use mithril_common::test::double::Dummy;
    use tokio::sync::RwLock;

    use super::*;

    /// This aggregator client is intended to be used by test services.
    /// It actually does not communicate with an aggregator host but mimics this behavior.
    /// It is driven by a Tester that controls the data it can return, and it can return its internal state for testing.
    pub struct DumbAggregatorClient {
        epoch_settings: RwLock<Option<RegisteredSigners>>,
    }

    impl Default for DumbAggregatorClient {
        fn default() -> Self {
            Self {
                epoch_settings: RwLock::new(Some(RegisteredSigners::dummy())),
            }
        }
    }

    #[async_trait]
    impl AggregatorClient for DumbAggregatorClient {
        async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
            let epoch_settings = self.epoch_settings.read().await.clone();

            Ok(epoch_settings)
        }
    }
}
