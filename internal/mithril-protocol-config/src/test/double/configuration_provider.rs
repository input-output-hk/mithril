//! provides test doubles for MithrilNetworkConfigurationProvider
use std::{collections::BTreeSet, sync::Arc};

use tokio::sync::RwLock;

use crate::{
    interface::MithrilNetworkConfigurationProvider,
    model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration},
};
use async_trait::async_trait;
use mithril_common::{
    StdResult,
    entities::{Epoch, ProtocolParameters, SignedEntityTypeDiscriminants},
};
use mithril_ticker::TickerService;

/// A fake [MithrilNetworkConfigurationProvider] that return [MithrilNetworkConfiguration]
pub struct FakeMithrilNetworkConfigurationProvider {
    /// The protocol parameters for the signer registration
    pub signer_registration_protocol_parameters: ProtocolParameters,

    /// The available signed entity types
    pub available_signed_entity_types: RwLock<BTreeSet<SignedEntityTypeDiscriminants>>,

    /// The configuration for each signed entity type
    pub signed_entity_types_config: SignedEntityTypeConfiguration,

    ticker_service: Arc<dyn TickerService>,
}

impl FakeMithrilNetworkConfigurationProvider {
    /// FakeMithrilNetworkConfigurationProvider factory
    pub fn new(
        signer_registration_protocol_parameters: ProtocolParameters,
        available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
        signed_entity_types_config: SignedEntityTypeConfiguration,
        ticker_service: Arc<dyn TickerService>,
    ) -> Self {
        Self {
            signer_registration_protocol_parameters,
            available_signed_entity_types: RwLock::new(available_signed_entity_types),
            signed_entity_types_config,
            ticker_service,
        }
    }

    /// Change the allowed signed entity discriminants (signed entity types) returned by the provider
    pub async fn change_allowed_discriminants(
        &self,
        discriminants: &BTreeSet<SignedEntityTypeDiscriminants>,
    ) {
        let mut available_signed_entity_types = self.available_signed_entity_types.write().await;
        *available_signed_entity_types = discriminants.clone();
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl MithrilNetworkConfigurationProvider for FakeMithrilNetworkConfigurationProvider {
    async fn get_network_configuration(
        &self,
        epoch: Epoch, //TODO
    ) -> StdResult<MithrilNetworkConfiguration> {
        let time_point = self.ticker_service.get_current_time_point().await?;
        let available_signed_entity_types = self.available_signed_entity_types.read().await;

        Ok(MithrilNetworkConfiguration {
            epoch: time_point.epoch,
            signer_registration_protocol_parameters: self
                .signer_registration_protocol_parameters
                .clone(),
            available_signed_entity_types: available_signed_entity_types.clone(),
            signed_entity_types_config: self.signed_entity_types_config.clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::BTreeSet, sync::Arc};

    use mithril_common::{
        entities::{
            BlockNumber, CardanoTransactionsSigningConfig, ChainPoint, Epoch, ProtocolParameters,
            SignedEntityTypeDiscriminants, TimePoint,
        },
        test::double::Dummy,
    };
    use mithril_ticker::MithrilTickerService;

    use crate::{
        interface::MithrilNetworkConfigurationProvider, model::SignedEntityTypeConfiguration,
        test::double::configuration_provider::FakeMithrilNetworkConfigurationProvider,
    };
    use mithril_cardano_node_chain::test::double::FakeChainObserver;
    use mithril_cardano_node_internal_database::test::double::DumbImmutableFileObserver;

    async fn ticker_service() -> Arc<MithrilTickerService> {
        let immutable_observer = Arc::new(DumbImmutableFileObserver::new());
        immutable_observer.shall_return(Some(1)).await;
        let chain_observer = Arc::new(FakeChainObserver::new(Some(TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 1,
            chain_point: ChainPoint::dummy(),
        })));

        Arc::new(MithrilTickerService::new(
            chain_observer.clone(),
            immutable_observer.clone(),
        ))
    }

    #[tokio::test]
    async fn test_get() {
        let signer_registration_protocol_parameters = ProtocolParameters {
            k: 2,
            m: 3,
            phi_f: 0.5,
        };
        let available_signed_entity_types = BTreeSet::from([
            SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            SignedEntityTypeDiscriminants::CardanoTransactions,
        ]);
        let signed_entity_types_config = SignedEntityTypeConfiguration {
            cardano_transactions: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(12),
                step: BlockNumber(10),
            }),
        };

        let mithril_network_configuration_provider = FakeMithrilNetworkConfigurationProvider::new(
            signer_registration_protocol_parameters.clone(),
            available_signed_entity_types.clone(),
            signed_entity_types_config.clone(),
            ticker_service().await,
        );

        let actual_config = mithril_network_configuration_provider
            .get_network_configuration(Epoch(1)) //TODO
            .await
            .unwrap();

        assert_eq!(actual_config.epoch, Epoch(1));
        assert_eq!(
            actual_config.signer_registration_protocol_parameters,
            signer_registration_protocol_parameters
        );
        assert_eq!(
            actual_config.available_signed_entity_types,
            available_signed_entity_types
        );
        assert_eq!(
            actual_config.signed_entity_types_config,
            signed_entity_types_config
        );
    }
}
