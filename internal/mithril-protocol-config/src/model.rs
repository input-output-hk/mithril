//! Model definitions for Mithril Protocol Configuration.

use std::collections::BTreeSet;

use mithril_common::{
    entities::{
        CardanoBlocksTransactionsSigningConfig, CardanoTransactionsSigningConfig, Epoch,
        ProtocolParameters, SignedEntityTypeDiscriminants,
    },
    messages::ProtocolConfigurationMessage,
};

#[derive(PartialEq, Clone, Debug)]

/// Custom configuration for the signed entity types
pub struct SignedEntityTypeConfiguration {
    /// Signing configuration for Cardano transactions
    pub cardano_transactions: Option<CardanoTransactionsSigningConfig>,

    /// Signing configuration for Cardano blocks and transactions
    pub cardano_blocks_transactions: Option<CardanoBlocksTransactionsSigningConfig>,
}

/// A Mithril network configuration
#[derive(PartialEq, Clone, Debug)]
pub struct MithrilNetworkConfiguration {
    /// Epoch
    pub epoch: Epoch,

    /// Configuration for aggregation
    pub configuration_for_aggregation: MithrilNetworkConfigurationForEpoch,

    /// Configuration for next aggregation
    pub configuration_for_next_aggregation: MithrilNetworkConfigurationForEpoch,

    /// Configuration for registration
    pub configuration_for_registration: MithrilNetworkConfigurationForEpoch,
}

//A epoch configuration
#[derive(PartialEq, Clone, Debug)]

/// A network configuration available for an epoch
pub struct MithrilNetworkConfigurationForEpoch {
    /// Cryptographic protocol parameters (`k`, `m` and `phi_f`)
    pub protocol_parameters: ProtocolParameters,

    /// List of available types of certifications
    pub enabled_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Custom configurations for signed entity types
    pub signed_entity_types_config: SignedEntityTypeConfiguration,
}

impl From<ProtocolConfigurationMessage> for MithrilNetworkConfigurationForEpoch {
    fn from(message: ProtocolConfigurationMessage) -> Self {
        MithrilNetworkConfigurationForEpoch {
            protocol_parameters: message.protocol_parameters,
            enabled_signed_entity_types: message.available_signed_entity_types,
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: message.cardano_transactions_signing_config,
                cardano_blocks_transactions: message.cardano_blocks_transactions_signing_config,
            },
        }
    }
}
