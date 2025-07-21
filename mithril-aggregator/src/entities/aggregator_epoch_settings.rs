use mithril_common::entities::{CardanoTransactionsSigningConfig, ProtocolParameters};

/// AggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct AggregatorEpochSettings {
    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Cardano transactions signing configuration
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
}
