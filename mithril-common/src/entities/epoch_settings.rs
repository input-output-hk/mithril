use crate::entities::{Epoch, ProtocolParameters};

/// EpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Default)]
pub struct EpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    pub next_protocol_parameters: ProtocolParameters,
}
