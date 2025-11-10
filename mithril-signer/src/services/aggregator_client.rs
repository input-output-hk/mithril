use async_trait::async_trait;

use mithril_aggregator_client::AggregatorHttpClient;
use mithril_aggregator_client::query::{GetAggregatorFeaturesQuery, GetEpochSettingsQuery};
use mithril_common::{
    StdResult,
    messages::{AggregatorFeaturesMessage, TryFromMessageAdapter},
};

use crate::entities::SignerEpochSettings;
use crate::message_adapters::FromEpochSettingsAdapter;

/// Trait for mocking and testing a `AggregatorClient`
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Retrieves epoch settings from the aggregator
    async fn retrieve_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>>;

    /// Retrieves aggregator features message from the aggregator
    async fn retrieve_aggregator_features(&self) -> StdResult<AggregatorFeaturesMessage>;
}

#[async_trait]
impl AggregatorClient for AggregatorHttpClient {
    async fn retrieve_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>> {
        let message = self.send(GetEpochSettingsQuery::current()).await?;
        let epoch_settings = FromEpochSettingsAdapter::try_adapt(message)?;

        Ok(Some(epoch_settings))
    }

    async fn retrieve_aggregator_features(&self) -> StdResult<AggregatorFeaturesMessage> {
        let aggregator_features = self.send(GetAggregatorFeaturesQuery::current()).await?;
        Ok(aggregator_features)
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
        epoch_settings: RwLock<Option<SignerEpochSettings>>,
        aggregator_features: RwLock<AggregatorFeaturesMessage>,
    }

    impl Default for DumbAggregatorClient {
        fn default() -> Self {
            Self {
                epoch_settings: RwLock::new(Some(SignerEpochSettings::dummy())),
                aggregator_features: RwLock::new(AggregatorFeaturesMessage::dummy()),
            }
        }
    }

    #[async_trait]
    impl AggregatorClient for DumbAggregatorClient {
        async fn retrieve_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>> {
            let epoch_settings = self.epoch_settings.read().await.clone();

            Ok(epoch_settings)
        }

        async fn retrieve_aggregator_features(&self) -> StdResult<AggregatorFeaturesMessage> {
            let aggregator_features = self.aggregator_features.read().await;
            Ok(aggregator_features.clone())
        }
    }
}
