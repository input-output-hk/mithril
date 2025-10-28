//! provides test doubles for MithrilNetworkConfigurationProvider

use tokio::sync::RwLock;

use crate::{
    interface::MithrilNetworkConfigurationProvider,
    model::{MithrilNetworkConfigurationForEpoch, MithrilNetworkConfiguration},
};
use async_trait::async_trait;
use mithril_common::{StdResult, entities::Epoch};

/// A fake [MithrilNetworkConfigurationProvider] that return [MithrilNetworkConfiguration]
pub struct FakeMithrilNetworkConfigurationProvider {
    /// Configuration for aggregation
    pub configuration_for_aggregation: RwLock<MithrilNetworkConfigurationForEpoch>,

    /// Configuration for next aggregation
    pub configuration_for_next_aggregation: RwLock<MithrilNetworkConfigurationForEpoch>,

    /// Configuration for registration
    pub configuration_for_registration: RwLock<MithrilNetworkConfigurationForEpoch>,
}

impl FakeMithrilNetworkConfigurationProvider {
    /// FakeMithrilNetworkConfigurationProvider factory
    pub fn new(
        configuration_for_aggregation: MithrilNetworkConfigurationForEpoch,
        configuration_for_next_aggregation: MithrilNetworkConfigurationForEpoch,
        configuration_for_registration: MithrilNetworkConfigurationForEpoch,
    ) -> Self {
        Self {
            configuration_for_aggregation: RwLock::new(configuration_for_aggregation),
            configuration_for_next_aggregation: RwLock::new(configuration_for_next_aggregation),
            configuration_for_registration: RwLock::new(configuration_for_registration),
        }
    }

    ///Change the configuration of the aggregation
    pub async fn change_aggregation_configuration(&self, conf: MithrilNetworkConfigurationForEpoch) {
        let mut configuration_for_aggregation = self.configuration_for_aggregation.write().await;
        *configuration_for_aggregation = conf;
    }
}

#[cfg_attr(target_family = "wasm", async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait)]
impl MithrilNetworkConfigurationProvider for FakeMithrilNetworkConfigurationProvider {
    async fn get_network_configuration(
        &self,
        epoch: Epoch,
    ) -> StdResult<MithrilNetworkConfiguration> {
        let configuration_for_aggregation = self.configuration_for_aggregation.read().await.clone();

        let configuration_for_next_aggregation =
            self.configuration_for_next_aggregation.read().await.clone();

        let configuration_for_registration =
            self.configuration_for_registration.read().await.clone();

        Ok(MithrilNetworkConfiguration {
            epoch,
            configuration_for_aggregation,
            configuration_for_next_aggregation,
            configuration_for_registration,
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{
        BlockNumber, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters,
        SignedEntityTypeDiscriminants,
    };

    use crate::{
        interface::MithrilNetworkConfigurationProvider,
        model::{MithrilNetworkConfigurationForEpoch, SignedEntityTypeConfiguration},
        test::double::configuration_provider::FakeMithrilNetworkConfigurationProvider,
    };

    #[tokio::test]
    async fn test_get_network_configuration() {
        let configuration_for_aggregation = MithrilNetworkConfigurationForEpoch {
            protocol_parameters: ProtocolParameters {
                k: 1,
                m: 11,
                phi_f: 0.1,
            },
            enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: Some(CardanoTransactionsSigningConfig {
                    step: BlockNumber(10),
                    security_parameter: BlockNumber(100),
                }),
            },
        };

        let configuration_for_next_aggregation = MithrilNetworkConfigurationForEpoch {
            protocol_parameters: ProtocolParameters {
                k: 2,
                m: 22,
                phi_f: 0.2,
            },
            enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: Some(CardanoTransactionsSigningConfig {
                    step: BlockNumber(20),
                    security_parameter: BlockNumber(200),
                }),
            },
        };

        let configuration_for_registration = MithrilNetworkConfigurationForEpoch {
            protocol_parameters: ProtocolParameters {
                k: 3,
                m: 33,
                phi_f: 0.3,
            },
            enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: Some(CardanoTransactionsSigningConfig {
                    step: BlockNumber(30),
                    security_parameter: BlockNumber(300),
                }),
            },
        };

        let mithril_network_configuration_provider = FakeMithrilNetworkConfigurationProvider::new(
            configuration_for_aggregation.clone(),
            configuration_for_next_aggregation.clone(),
            configuration_for_registration.clone(),
        );

        let actual_config = mithril_network_configuration_provider
            .get_network_configuration(Epoch(1))
            .await
            .unwrap();

        assert_eq!(actual_config.epoch, Epoch(1));
        assert_eq!(
            actual_config.configuration_for_aggregation,
            configuration_for_aggregation
        );
        assert_eq!(
            actual_config.configuration_for_next_aggregation,
            configuration_for_next_aggregation
        );
        assert_eq!(
            actual_config.configuration_for_registration,
            configuration_for_registration
        );
    }
}
