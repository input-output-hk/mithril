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
}
