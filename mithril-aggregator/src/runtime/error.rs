use crate::ProtocolError;

use mithril_common::certificate_chain::CertificateVerifierError;
use mithril_common::chain_observer::ChainObserverError;
use mithril_common::digesters::ImmutableDigesterError;
use mithril_common::entities::BeaconComparisonError;
use mithril_common::store::StoreError;
use mithril_common::BeaconProviderError;
use std::error::Error as StdError;
use thiserror::Error;

/// Error encountered or produced by the Runtime.
/// This enum represents the faith of the errors produced during the state
/// transitions.
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// Errors that need the runtime to try again without changing its state.
    #[error("An error occured: {message}. This runtime cycle will be skipped. Nested error: {nested_error:#?}.")]
    KeepState {
        /// error message
        message: String,

        /// Eventual caught error
        nested_error: Option<Box<dyn StdError + Sync + Send>>,
    },
    /// A Critical error means the Runtime stops and the software exits with an
    /// error code.
    #[error("Critical error:'{message}'. Nested error: {nested_error:#?}.")]
    Critical {
        /// error message
        message: String,

        /// Eventual caught error
        nested_error: Option<Box<dyn StdError + Sync + Send>>,
    },
}

impl RuntimeError {
    /// Create a new KeepState error
    pub fn keep_state(message: &str, error: Option<Box<dyn StdError + Sync + Send>>) -> Self {
        Self::KeepState {
            message: message.to_string(),
            nested_error: error,
        }
    }

    /// Create a new Critical error
    pub fn critical(message: &str, error: Option<Box<dyn StdError + Sync + Send>>) -> Self {
        Self::Critical {
            message: message.to_string(),
            nested_error: error,
        }
    }
}

impl From<BeaconComparisonError> for RuntimeError {
    fn from(value: BeaconComparisonError) -> Self {
        Self::KeepState {
            message: "Beacon comparison error".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<ImmutableDigesterError> for RuntimeError {
    fn from(value: ImmutableDigesterError) -> Self {
        Self::KeepState {
            message: "Error while reading immutable files".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<ProtocolError> for RuntimeError {
    fn from(value: ProtocolError) -> Self {
        Self::KeepState {
            message: "Mithril Protocol Error".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<ChainObserverError> for RuntimeError {
    fn from(value: ChainObserverError) -> Self {
        Self::KeepState {
            message: "Chain observer error".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<StoreError> for RuntimeError {
    fn from(value: StoreError) -> Self {
        Self::KeepState {
            message: "Store Error".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<BeaconProviderError> for RuntimeError {
    fn from(value: BeaconProviderError) -> Self {
        Self::KeepState {
            message: "Could not read beacon from chain".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<CertificateVerifierError> for RuntimeError {
    fn from(value: CertificateVerifierError) -> Self {
        Self::KeepState {
            message: "Could not verify given certificate".to_string(),
            nested_error: Some(value.into()),
        }
    }
}
