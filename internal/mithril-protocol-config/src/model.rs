//! Model definitions for Mithril Protocol Configuration.

use std::collections::BTreeSet;

use mithril_common::{
    entities::{
        CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignedEntityTypeDiscriminants,
    },
    messages::ProtocolConfigurationMessage,
};

#[derive(PartialEq, Clone, Debug)]

/// Custom configuration for the signed entity types
pub struct SignedEntityTypeConfiguration {
    /// Cardano Transactions
    pub cardano_transactions: Option<CardanoTransactionsSigningConfig>,
}

/// A Mithril network configuration
#[derive(PartialEq, Clone, Debug)]
pub struct MithrilNetworkConfiguration {
    /// Epoch
    pub epoch: Epoch,

    /// Configuration for aggregation
    pub configuration_for_aggregation: EpochConfiguration,

    /// Configuration for next aggregation
    pub configuration_for_next_aggregation: EpochConfiguration,

    /// Configuration for registration
    pub configuration_for_registration: EpochConfiguration,
}

//A epoch configuration
#[derive(PartialEq, Clone, Debug)]

/// A network configuration available for an epoch
pub struct EpochConfiguration {
    /// Cryptographic protocol parameters (`k`, `m` and `phi_f`)
    pub protocol_parameters: ProtocolParameters,

    /// List of available types of certifications
    pub enabled_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Custom configurations for signed entity types
    pub signed_entity_types_config: SignedEntityTypeConfiguration,
}

impl From<ProtocolConfigurationMessage> for EpochConfiguration {
    fn from(message: ProtocolConfigurationMessage) -> Self {
        EpochConfiguration {
            protocol_parameters: message.protocol_parameters,
            enabled_signed_entity_types: message.available_signed_entity_types,
            signed_entity_types_config: SignedEntityTypeConfiguration {
                cardano_transactions: message.cardano_transactions_signing_config,
            },
        }
    }
}
