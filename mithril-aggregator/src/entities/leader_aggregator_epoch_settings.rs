use mithril_common::entities::{Epoch, Signer};

/// LeaderAggregatorEpochSettings represents the settings of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct LeaderAggregatorEpochSettings {
    /// Current Epoch
    pub epoch: Epoch,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,
}
