use async_trait::async_trait;

use mithril_aggregator_client::AggregatorHttpClient;
use mithril_aggregator_client::query::{
    GetAggregatorFeaturesQuery, GetEpochSettingsQuery, PostRegisterSignerQuery,
};
use mithril_common::{
    StdResult,
    entities::{Epoch, Signer},
    messages::{AggregatorFeaturesMessage, TryFromMessageAdapter, TryToMessageAdapter},
};

use crate::entities::SignerEpochSettings;
use crate::message_adapters::{FromEpochSettingsAdapter, ToRegisterSignerMessageAdapter};

/// Trait for mocking and testing a `AggregatorClient`
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AggregatorClient: Sync + Send {
    /// Retrieves epoch settings from the aggregator
    async fn retrieve_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>>;

    /// Registers signer with the aggregator.
    async fn register_signer(&self, epoch: Epoch, signer: &Signer) -> StdResult<()>;

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

    async fn register_signer(&self, epoch: Epoch, signer: &Signer) -> StdResult<()> {
        let register_signer_message =
            ToRegisterSignerMessageAdapter::try_adapt((epoch, signer.to_owned()))?;

        self.send(PostRegisterSignerQuery::new(register_signer_message))
            .await?;

        Ok(())
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
        last_registered_signer: RwLock<Option<Signer>>,
        aggregator_features: RwLock<AggregatorFeaturesMessage>,
        total_registered_signers: RwLock<u32>,
    }

    impl DumbAggregatorClient {
        /// Return the last signer that called with the `register` method.
        pub async fn get_last_registered_signer(&self) -> Option<Signer> {
            self.last_registered_signer.read().await.clone()
        }

        /// Return the total number of signers that called with the `register` method.
        pub async fn get_total_registered_signers(&self) -> u32 {
            *self.total_registered_signers.read().await
        }
    }

    impl Default for DumbAggregatorClient {
        fn default() -> Self {
            Self {
                epoch_settings: RwLock::new(Some(SignerEpochSettings::dummy())),
                last_registered_signer: RwLock::new(None),
                aggregator_features: RwLock::new(AggregatorFeaturesMessage::dummy()),
                total_registered_signers: RwLock::new(0),
            }
        }
    }

    #[async_trait]
    impl AggregatorClient for DumbAggregatorClient {
        async fn retrieve_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>> {
            let epoch_settings = self.epoch_settings.read().await.clone();

            Ok(epoch_settings)
        }

        /// Registers signer with the aggregator
        async fn register_signer(&self, _epoch: Epoch, signer: &Signer) -> StdResult<()> {
            let mut last_registered_signer = self.last_registered_signer.write().await;
            let signer = signer.clone();
            *last_registered_signer = Some(signer);

            let mut total_registered_signers = self.total_registered_signers.write().await;
            *total_registered_signers += 1;

            Ok(())
        }

        async fn retrieve_aggregator_features(&self) -> StdResult<AggregatorFeaturesMessage> {
            let aggregator_features = self.aggregator_features.read().await;
            Ok(aggregator_features.clone())
        }
    }
}
