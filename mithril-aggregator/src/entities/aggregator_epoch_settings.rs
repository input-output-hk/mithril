use mithril_common::entities::{CardanoTransactionsSigningConfig, ProtocolParameters};

/// AggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct AggregatorEpochSettings {
    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Cardano transactions signing configuration
    pub cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
}

impl AggregatorEpochSettings {
    #[cfg(test)]
    /// Create a dummy AggregatorEpochSettings
    pub fn dummy() -> AggregatorEpochSettings {
        use mithril_common::test_utils::fake_data;

        let protocol_parameters = fake_data::protocol_parameters();
        let cardano_transactions_signing_config = CardanoTransactionsSigningConfig::dummy();

        // Aggregator Epoch settings
        AggregatorEpochSettings {
            protocol_parameters,
            cardano_transactions_signing_config,
        }
    }
}
