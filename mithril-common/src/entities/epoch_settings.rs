use crate::entities::{Epoch, ProtocolParameters};

use super::Signer;

/// EpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq, Default)]
pub struct EpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Protocol parameters
    pub protocol_parameters: ProtocolParameters,

    /// Next Protocol parameters
    pub next_protocol_parameters: ProtocolParameters,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,
}
