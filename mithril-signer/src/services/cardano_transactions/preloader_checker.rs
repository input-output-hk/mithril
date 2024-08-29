use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    cardano_transactions_preloader::CardanoTransactionsPreloaderChecker,
    entities::SignedEntityTypeDiscriminants, StdResult,
};

use crate::services::AggregatorClient;

/// CardanoTransactionsPreloaderActivationSigner
pub struct CardanoTransactionsPreloaderActivationSigner {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl CardanoTransactionsPreloaderActivationSigner {
    /// Create a new instance of `CardanoTransactionsPreloaderActivationSigner`
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }
}

#[async_trait]
impl CardanoTransactionsPreloaderChecker for CardanoTransactionsPreloaderActivationSigner {
    async fn is_activated(&self) -> StdResult<bool> {
        let message = self
            .aggregator_client
            .retrieve_aggregator_features()
            .await
            .with_context(|| "An error occurred while calling the Aggregator")?;

        let activated_signed_entity_types = message.capabilities.signed_entity_types;

        Ok(activated_signed_entity_types
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions))
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::collections::BTreeSet;

    use mithril_common::{
        entities::SignedEntityTypeDiscriminants, messages::AggregatorFeaturesMessage,
    };

    use crate::services::{AggregatorClientError, MockAggregatorClient};

    use super::*;

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_cardano_transactions_not_in_aggregator_capabilities(
    ) {
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_aggregator_features()
            .times(1)
            .returning(|| {
                let mut message = AggregatorFeaturesMessage::dummy();
                message.capabilities.signed_entity_types =
                    BTreeSet::from([SignedEntityTypeDiscriminants::MithrilStakeDistribution]);
                Ok(message)
            });
        let preloader =
            CardanoTransactionsPreloaderActivationSigner::new(Arc::new(aggregator_client));

        let is_activated = preloader.is_activated().await.unwrap();

        assert!(!is_activated);
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_cardano_transactions_in_aggregator_capabilities(
    ) {
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_aggregator_features()
            .times(1)
            .returning(|| {
                let mut message = AggregatorFeaturesMessage::dummy();
                message.capabilities.signed_entity_types =
                    BTreeSet::from([SignedEntityTypeDiscriminants::CardanoTransactions]);
                Ok(message)
            });
        let preloader =
            CardanoTransactionsPreloaderActivationSigner::new(Arc::new(aggregator_client));

        let is_activated = preloader.is_activated().await.unwrap();

        assert!(is_activated);
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_aggregator_call_fails() {
        let mut aggregator_client = MockAggregatorClient::new();
        aggregator_client
            .expect_retrieve_aggregator_features()
            .times(1)
            .returning(|| {
                Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                    "Aggregator call failed"
                )))
            });
        let preloader =
            CardanoTransactionsPreloaderActivationSigner::new(Arc::new(aggregator_client));

        preloader
            .is_activated()
            .await
            .expect_err("Should fail due to aggregator call failure");
    }
}
