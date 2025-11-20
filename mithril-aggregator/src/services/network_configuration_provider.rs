use anyhow::Context;
use async_trait::async_trait;
use std::collections::BTreeSet;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_common::entities::{Epoch, SignedEntityTypeDiscriminants};
use mithril_protocol_config::interface::MithrilNetworkConfigurationProvider;
use mithril_protocol_config::model::{
    MithrilNetworkConfiguration, MithrilNetworkConfigurationForEpoch, SignedEntityTypeConfiguration,
};

use crate::EpochSettingsStorer;
use crate::entities::AggregatorEpochSettings;

/// Read network configuration from the database epoch_settings or, if no records is available for
/// an epoch, fallback to the provided configuration value
pub struct LocalMithrilNetworkConfigurationProvider {
    local_configuration_epoch_settings: AggregatorEpochSettings,
    allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    epoch_settings_store: Arc<dyn EpochSettingsStorer>,
}

impl LocalMithrilNetworkConfigurationProvider {
    /// Instantiate a new `LocalMithrilNetworkConfigurationProvider`
    pub fn new(
        local_configuration_epoch_settings: AggregatorEpochSettings,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
        epoch_settings_store: Arc<dyn EpochSettingsStorer>,
    ) -> Self {
        Self {
            local_configuration_epoch_settings,
            allowed_discriminants,
            epoch_settings_store,
        }
    }

    /// Get epoch configuration from store or fallback to local configuration if it does not exists
    async fn get_stored_configuration_or_fallback(
        &self,
        epoch: Epoch,
    ) -> StdResult<MithrilNetworkConfigurationForEpoch> {
        let epoch_settings = self.epoch_settings_store.get_epoch_settings(epoch).await?.unwrap_or(
            AggregatorEpochSettings {
                protocol_parameters: self
                    .local_configuration_epoch_settings
                    .protocol_parameters
                    .clone(),
                cardano_transactions_signing_config: self
                    .local_configuration_epoch_settings
                    .cardano_transactions_signing_config
                    .clone(),
            },
        );

        Ok(MithrilNetworkConfigurationForEpoch {
            enabled_signed_entity_types: self.allowed_discriminants.clone(),
            protocol_parameters: epoch_settings.protocol_parameters,
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: epoch_settings.cardano_transactions_signing_config,
            },
        })
    }
}

#[async_trait]
impl MithrilNetworkConfigurationProvider for LocalMithrilNetworkConfigurationProvider {
    async fn get_network_configuration(
        &self,
        epoch: Epoch,
    ) -> StdResult<MithrilNetworkConfiguration> {
        let aggregation_epoch =
            epoch.offset_to_signer_retrieval_epoch().with_context(|| {
                format!("MithrilNetworkConfigurationProvider could not compute aggregation epoch from epoch: {epoch}")
            })?;
        let next_aggregation_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let registration_epoch = epoch.offset_to_next_signer_retrieval_epoch().next();

        let configuration_for_aggregation =
            self.get_stored_configuration_or_fallback(aggregation_epoch).await?;
        let configuration_for_next_aggregation = self
            .get_stored_configuration_or_fallback(next_aggregation_epoch)
            .await?;
        let configuration_for_registration =
            self.get_stored_configuration_or_fallback(registration_epoch).await?;

        let config = MithrilNetworkConfiguration {
            epoch,
            configuration_for_aggregation,
            configuration_for_next_aggregation,
            configuration_for_registration,
        };
        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{BlockNumber, CardanoTransactionsSigningConfig, ProtocolParameters},
        test::double::Dummy,
    };

    use crate::store::FakeEpochSettingsStorer;

    use super::*;

    #[tokio::test]
    async fn get_stored_configuration_with_stored_value_returns_them() {
        let local_configuration_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
            ..Dummy::dummy()
        };
        let stored_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(1000, 100, 0.1),
            ..Dummy::dummy()
        };

        let local_provider = LocalMithrilNetworkConfigurationProvider::new(
            local_configuration_epoch_settings,
            SignedEntityTypeDiscriminants::all(),
            Arc::new(FakeEpochSettingsStorer::new(vec![(
                Epoch(42),
                stored_epoch_settings.clone(),
            )])),
        );

        let network_configuration = local_provider
            .get_stored_configuration_or_fallback(Epoch(42))
            .await
            .unwrap();

        assert_eq!(
            stored_epoch_settings.protocol_parameters,
            network_configuration.protocol_parameters
        )
    }

    #[tokio::test]
    async fn get_stored_configuration_without_stored_value_fallback_to_configuration_value() {
        let local_configuration_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
            ..Dummy::dummy()
        };

        let local_provider = LocalMithrilNetworkConfigurationProvider::new(
            local_configuration_epoch_settings.clone(),
            SignedEntityTypeDiscriminants::all(),
            Arc::new(FakeEpochSettingsStorer::new(vec![])),
        );

        let network_configuration = local_provider
            .get_stored_configuration_or_fallback(Epoch(42))
            .await
            .unwrap();

        assert_eq!(
            local_configuration_epoch_settings.protocol_parameters,
            network_configuration.protocol_parameters
        )
    }

    #[tokio::test]
    async fn test_get_network_configuration_retrieve_configurations_for_aggregation_next_aggregation_and_registration()
     {
        let local_configuration_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: ProtocolParameters::new(3000, 300, 0.3),
            cardano_transactions_signing_config: Some(CardanoTransactionsSigningConfig {
                security_parameter: BlockNumber(3),
                step: BlockNumber(30),
            }),
        };

        // Nothing stored at 44, should fallback to configuration
        let local_provider = LocalMithrilNetworkConfigurationProvider::new(
            local_configuration_epoch_settings,
            SignedEntityTypeDiscriminants::all(),
            Arc::new(FakeEpochSettingsStorer::new(vec![
                (
                    Epoch(42),
                    AggregatorEpochSettings {
                        protocol_parameters: ProtocolParameters::new(1000, 100, 0.1),
                        cardano_transactions_signing_config: Some(
                            CardanoTransactionsSigningConfig {
                                security_parameter: BlockNumber(1),
                                step: BlockNumber(10),
                            },
                        ),
                    },
                ),
                (
                    Epoch(43),
                    AggregatorEpochSettings {
                        protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
                        cardano_transactions_signing_config: Some(
                            CardanoTransactionsSigningConfig {
                                security_parameter: BlockNumber(2),
                                step: BlockNumber(20),
                            },
                        ),
                    },
                ),
            ])),
        );

        let configuration = local_provider.get_network_configuration(Epoch(43)).await.unwrap();

        assert_eq!(Epoch(43), configuration.epoch);

        assert_eq!(
            MithrilNetworkConfigurationForEpoch {
                protocol_parameters: ProtocolParameters::new(1000, 100, 0.1),
                enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
                signed_entity_types_config: SignedEntityTypeConfiguration {
                    cardano_transactions: Some(CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumber(1),
                        step: BlockNumber(10),
                    }),
                },
            },
            configuration.configuration_for_aggregation
        );

        assert_eq!(
            MithrilNetworkConfigurationForEpoch {
                protocol_parameters: ProtocolParameters::new(2000, 200, 0.2),
                enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
                signed_entity_types_config: SignedEntityTypeConfiguration {
                    cardano_transactions: Some(CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumber(2),
                        step: BlockNumber(20),
                    }),
                },
            },
            configuration.configuration_for_next_aggregation
        );

        assert_eq!(
            MithrilNetworkConfigurationForEpoch {
                protocol_parameters: ProtocolParameters::new(3000, 300, 0.3),
                enabled_signed_entity_types: SignedEntityTypeDiscriminants::all(),
                signed_entity_types_config: SignedEntityTypeConfiguration {
                    cardano_transactions: Some(CardanoTransactionsSigningConfig {
                        security_parameter: BlockNumber(3),
                        step: BlockNumber(30),
                    }),
                },
            },
            configuration.configuration_for_registration
        );
    }
}
