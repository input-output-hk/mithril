use mithril_common::entities::ProtocolParameters;

/// AggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Default)]
pub struct AggregatorEpochSettings {
    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,
}

impl AggregatorEpochSettings {
    #[cfg(test)]
    /// Create a dummy AggregatorEpochSettings
    pub fn dummy() -> AggregatorEpochSettings {
        use mithril_common::test_utils::fake_data;

        // Protocol parameters
        let protocol_parameters = fake_data::protocol_parameters();

        // Aggregator Epoch settings
        AggregatorEpochSettings {
            protocol_parameters,
        }
    }
}
