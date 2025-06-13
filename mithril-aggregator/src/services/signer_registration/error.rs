use thiserror::Error;

use mithril_common::{
    entities::{Epoch, SignerWithStake},
    StdError,
};

use mithril_cardano_node_chain::chain_observer::ChainObserverError;

/// Error type for signer registerer service.
#[derive(Error, Debug)]
pub enum SignerRegistrationError {
    /// No signer registration round opened yet
    #[error("a signer registration round is not opened yet, please try again later")]
    RegistrationRoundNotYetOpened,

    /// Registration round for unexpected epoch
    #[error("unexpected signer registration round epoch: current_round_epoch: {current_round_epoch}, received_epoch: {received_epoch}")]
    RegistrationRoundUnexpectedEpoch {
        /// Epoch of the current round
        current_round_epoch: Epoch,
        /// Epoch of the received signer registration
        received_epoch: Epoch,
    },

    /// Chain observer error.
    #[error("chain observer error")]
    ChainObserver(#[from] ChainObserverError),

    /// Signer is already registered.
    #[error("signer already registered")]
    ExistingSigner(Box<SignerWithStake>),

    /// Store.
    #[error("store error")]
    Store(#[source] StdError),

    /// Epoch service.
    #[error("epoch service error")]
    EpochService(#[source] StdError),

    /// Signer registration failed.
    #[error("signer registration failed")]
    FailedSignerRegistration(#[source] StdError),

    /// Signer recorder failed.
    #[error("signer recorder failed: '{0}'")]
    FailedSignerRecorder(String),

    /// Signer registration is always closed on a follower aggregator.
    #[error("signer registration is always closed on a follower aggregator")]
    RegistrationRoundAlwaysClosedOnFollowerAggregator,

    /// Signer synchronization is not available on a leader aggregator.
    #[error("signer synchronization is not available on a leader aggregator")]
    SignerSynchronizationUnavailableOnLeaderAggregator,

    /// Failed fetching leader aggregator epoch settings.
    #[error("failed fetching leader aggregator epoch settings: '{0}'")]
    FailedFetchingLeaderAggregatorEpochSettings(#[source] StdError),
}
