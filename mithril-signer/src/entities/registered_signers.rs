use mithril_common::entities::{Epoch, Signer};

/// `RegisteredSigners` represents the registered signers of an epoch
#[derive(Clone, Debug, PartialEq)]
pub struct RegisteredSigners {
    /// Epoch for which those registrations are active.
    pub epoch: Epoch,

    /// Current Signers
    pub current_signers: Vec<Signer>,

    /// Signers that will be able to sign on the next epoch
    pub next_signers: Vec<Signer>,
}
