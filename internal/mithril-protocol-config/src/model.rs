//! Model definitions for Mithril Protocol Configuration.

use std::collections::BTreeSet;

use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignedEntityTypeDiscriminants,
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

    /// Cryptographic protocol parameters (`k`, `m` and `phi_f`)
    pub signer_registration_protocol_parameters: ProtocolParameters,

    /// List of available types of certifications
    pub enabled_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,

    /// Custom configurations for signed entity types
    pub signed_entity_types_config: SignedEntityTypeConfiguration,
}
