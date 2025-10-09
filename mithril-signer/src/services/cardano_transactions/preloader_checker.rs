use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{StdResult, entities::SignedEntityTypeDiscriminants};
use mithril_protocol_config::interface::MithrilNetworkConfigurationProvider;
use mithril_signed_entity_preloader::CardanoTransactionsPreloaderChecker;
/// CardanoTransactionsPreloaderActivationSigner
pub struct CardanoTransactionsPreloaderActivationSigner {
    network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider>,
}

impl CardanoTransactionsPreloaderActivationSigner {
    /// Create a new instance of `CardanoTransactionsPreloaderActivationSigner`
    pub fn new(
        network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider>,
    ) -> Self {
        Self {
            network_configuration_provider,
        }
    }
}

#[async_trait]
impl CardanoTransactionsPreloaderChecker for CardanoTransactionsPreloaderActivationSigner {
    async fn is_activated(&self) -> StdResult<bool> {
        let configuration = self
            .network_configuration_provider
            .get_network_configuration()
            .await
            .context("An error occurred while retrieving Mithril network configuration")?;

        let activated_signed_entity_types = configuration.available_signed_entity_types;

        Ok(activated_signed_entity_types
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions))
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mithril_common::{entities::SignedEntityTypeDiscriminants, test::double::Dummy};
    use mithril_protocol_config::model::MithrilNetworkConfiguration;
    use mockall::mock;
    use std::collections::BTreeSet;

    use super::*;

    mock! {
        pub MithrilNetworkConfigurationProvider {}

        #[async_trait]
        impl MithrilNetworkConfigurationProvider for MithrilNetworkConfigurationProvider {
            async fn get_network_configuration(&self) -> StdResult<MithrilNetworkConfiguration>;
        }
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_cardano_transactions_not_in_aggregator_capabilities()
     {
        let mut network_configuration_provider = MockMithrilNetworkConfigurationProvider::new();
        network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|| {
                Ok(MithrilNetworkConfiguration {
                    available_signed_entity_types: BTreeSet::from([
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    ]),
                    ..Dummy::dummy()
                })
            });

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(Arc::new(
            network_configuration_provider,
        ));

        let is_activated = preloader.is_activated().await.unwrap();

        assert!(!is_activated);
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_cardano_transactions_in_aggregator_capabilities()
     {
        let mut network_configuration_provider = MockMithrilNetworkConfigurationProvider::new();
        network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|| {
                Ok(MithrilNetworkConfiguration {
                    available_signed_entity_types: BTreeSet::from([
                        SignedEntityTypeDiscriminants::CardanoTransactions,
                    ]),
                    ..Dummy::dummy()
                })
            });

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(Arc::new(
            network_configuration_provider,
        ));

        let is_activated = preloader.is_activated().await.unwrap();

        assert!(is_activated);
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_aggregator_call_fails() {
        let mut network_configuration_provider = MockMithrilNetworkConfigurationProvider::new();
        network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|| Err(anyhow!("Aggregator call failure")));

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(Arc::new(
            network_configuration_provider,
        ));

        preloader
            .is_activated()
            .await
            .expect_err("Should fail due to aggregator call failure");
    }
}
