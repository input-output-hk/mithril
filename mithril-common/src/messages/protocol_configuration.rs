use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::entities::{
    CardanoTransactionsSigningConfig, ProtocolParameters, SignedEntityTypeDiscriminants,
};

/// ProtocolConfiguration represents the protocol configuration of an epoch
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ProtocolConfigurationMessage {
    /// Signer Registration Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Cardano transactions signing configuration for the current epoch
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,

    /// Aggregator enabled signed entity types
    pub available_signed_entity_types: BTreeSet<SignedEntityTypeDiscriminants>,
}
