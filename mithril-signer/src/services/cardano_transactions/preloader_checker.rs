use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{StdResult, entities::SignedEntityTypeDiscriminants};
use mithril_protocol_config::interface::MithrilNetworkConfigurationProvider;
use mithril_signed_entity_preloader::CardanoTransactionsPreloaderChecker;
use mithril_ticker::TickerService;
/// CardanoTransactionsPreloaderActivationSigner
pub struct CardanoTransactionsPreloaderActivationSigner {
    network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider>,
    ticker_service: Arc<dyn TickerService>,
}

impl CardanoTransactionsPreloaderActivationSigner {
    /// Create a new instance of `CardanoTransactionsPreloaderActivationSigner`
    pub fn new(
        network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider>,
        ticker_service: Arc<dyn TickerService>,
    ) -> Self {
        Self {
            network_configuration_provider,
            ticker_service,
        }
    }
}

#[async_trait]
impl CardanoTransactionsPreloaderChecker for CardanoTransactionsPreloaderActivationSigner {
    async fn is_activated(&self) -> StdResult<bool> {
        let epoch = self.ticker_service.get_current_epoch().await?;

        let configuration = self
            .network_configuration_provider
            .get_network_configuration(epoch)
            .await
            .context(format!(
                "An error occurred while retrieving Mithril network configuration for epoch {epoch}"
            ))?;

        let activated_signed_entity_types = configuration.available_signed_entity_types;

        Ok(activated_signed_entity_types
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions))
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mithril_common::{
        entities::{Epoch, SignedEntityTypeDiscriminants, TimePoint},
        test::double::Dummy,
    };
    use mithril_protocol_config::model::MithrilNetworkConfiguration;
    use mockall::mock;
    use std::collections::BTreeSet;

    use super::*;

    mock! {
        pub MithrilNetworkConfigurationProvider {}

        #[async_trait]
        impl MithrilNetworkConfigurationProvider for MithrilNetworkConfigurationProvider {
            async fn get_network_configuration(&self, epoch: Epoch) -> StdResult<MithrilNetworkConfiguration>;
        }
    }
    mock! {
        pub TickerService {}

        #[async_trait]
        impl TickerService for TickerService {
            async fn get_current_time_point(&self) -> StdResult<TimePoint>;
            async fn get_current_epoch(&self) -> StdResult<Epoch>;
        }
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_cardano_transactions_not_in_aggregator_capabilities()
     {
        let mut network_configuration_provider = MockMithrilNetworkConfigurationProvider::new();
        network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|_| {
                Ok(MithrilNetworkConfiguration {
                    available_signed_entity_types: BTreeSet::from([
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                    ]),
                    ..Dummy::dummy()
                })
            });
        let mut ticker_service = MockTickerService::new();
        ticker_service
            .expect_get_current_epoch()
            .times(1)
            .returning(|| Ok(Epoch(1)));

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(
            Arc::new(network_configuration_provider),
            Arc::new(ticker_service),
        );

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
            .returning(|_| {
                Ok(MithrilNetworkConfiguration {
                    available_signed_entity_types: BTreeSet::from([
                        SignedEntityTypeDiscriminants::CardanoTransactions,
                    ]),
                    ..Dummy::dummy()
                })
            });

        let mut ticker_service = MockTickerService::new();
        ticker_service
            .expect_get_current_epoch()
            .times(1)
            .returning(|| Ok(Epoch(1)));

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(
            Arc::new(network_configuration_provider),
            Arc::new(ticker_service),
        );

        let is_activated = preloader.is_activated().await.unwrap();

        assert!(is_activated);
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_aggregator_call_fails() {
        let mut network_configuration_provider = MockMithrilNetworkConfigurationProvider::new();
        network_configuration_provider
            .expect_get_network_configuration()
            .times(1)
            .returning(|_| Err(anyhow!("Aggregator call failure")));

        let mut ticker_service = MockTickerService::new();
        ticker_service
            .expect_get_current_epoch()
            .times(1)
            .returning(|| Ok(Epoch(1)));

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(
            Arc::new(network_configuration_provider),
            Arc::new(ticker_service),
        );

        preloader
            .is_activated()
            .await
            .expect_err("Should fail due to aggregator call failure");
    }

    #[tokio::test]
    async fn preloader_activation_state_activate_preloader_when_ticker_service_call_fails() {
        let network_configuration_provider = MockMithrilNetworkConfigurationProvider::new();

        let mut ticker_service = MockTickerService::new();
        ticker_service
            .expect_get_current_epoch()
            .times(1)
            .returning(|| Err(anyhow!("Ticker service call failure")));

        let preloader = CardanoTransactionsPreloaderActivationSigner::new(
            Arc::new(network_configuration_provider),
            Arc::new(ticker_service),
        );

        preloader
            .is_activated()
            .await
            .expect_err("Should fail due to ticker service call failure");
    }
}
