use mithril_common::entities::{CardanoTransactionsSigningConfig, ProtocolParameters};

/// AggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct AggregatorEpochSettings {
    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Cardano transactions signing configuration
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
}

#[cfg(test)]
mod test_utils {
    use std::collections::BTreeSet;

    use mithril_common::entities::SignedEntityTypeDiscriminants;
    use mithril_protocol_config::model::{
        MithrilNetworkConfigurationForEpoch, SignedEntityTypeConfiguration,
    };

    use super::*;

    impl AggregatorEpochSettings {
        /// Convert this epoch setting into a [MithrilNetworkConfigurationForEpoch]
        pub fn into_network_configuration_for_epoch(
            self,
            enabled_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
        ) -> MithrilNetworkConfigurationForEpoch {
            MithrilNetworkConfigurationForEpoch {
                protocol_parameters: self.protocol_parameters,
                enabled_signed_entity_types,
                signed_entity_types_config: SignedEntityTypeConfiguration {
                    cardano_transactions: Some(self.cardano_transactions_signing_config),
                },
            }
        }
    }
}
