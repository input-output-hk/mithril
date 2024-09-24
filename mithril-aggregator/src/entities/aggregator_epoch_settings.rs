use mithril_common::entities::ProtocolParameters;

/// AggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Default)]
pub struct AggregatorEpochSettings {
    /// Protocol parameters
    pub protocol_parameters: ProtocolParameters,
}
