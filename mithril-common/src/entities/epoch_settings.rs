use crate::entities::{Epoch, ProtocolParameters};
use serde::{Deserialize, Serialize};

/// EpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct EpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Protocol parameters
    #[serde(rename = "protocol")]
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    #[serde(rename = "next_protocol")]
    pub next_protocol_parameters: ProtocolParameters,
}
