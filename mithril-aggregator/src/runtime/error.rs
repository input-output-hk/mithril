use mithril_common::StdError;
use thiserror::Error;

/// Error encountered or produced by the Runtime.
/// This enum represents the faith of the errors produced during the state
/// transitions.
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// Errors that need the runtime to try again without changing its state.
    #[error("An error occured: {message}. This runtime cycle will be skipped.")]
    KeepState {
        /// error message
        message: String,

        /// Eventual caught error
        #[source]
        nested_error: Option<StdError>,
    },
    /// A Critical error means the Runtime stops and the software exits with an
    /// error code.
    #[error("Critical error:'{message}'.")]
    Critical {
        /// error message
        message: String,

        /// Eventual caught error
        #[source]
        nested_error: Option<StdError>,
    },
    /// An error that needs to re-initialize the state machine.
    #[error("An error occured: {message}. The state machine will be re-initialized.")]
    ReInit {
        /// error message
        message: String,

        /// Eventual caught error
        #[source]
        nested_error: Option<StdError>,
    },
}

impl RuntimeError {
    /// Create a new KeepState error
    pub fn keep_state(message: &str, error: Option<StdError>) -> Self {
        Self::KeepState {
            message: message.to_string(),
            nested_error: error,
        }
    }

    /// Create a new Critical error
    pub fn critical(message: &str, error: Option<StdError>) -> Self {
        Self::Critical {
            message: message.to_string(),
            nested_error: error,
        }
    }
}

impl From<StdError> for RuntimeError {
    fn from(value: StdError) -> Self {
        Self::KeepState {
            message: "Error caught, state preserved, will retry to cycle.".to_string(),
            nested_error: Some(value),
        }
    }
}

/// Errors returned when the runner cannot fulfil its missions with no subsystem
/// to fail.
#[derive(Debug, Error)]
// TODO: Are these errors still relevant, do we need to remove them?
#[allow(clippy::enum_variant_names)]
pub enum RunnerError {
    /// No stake distribution found
    #[error("Missing stake distribution: '{0}'.")]
    MissingStakeDistribution(String),

    /// Missing protocol parameters
    #[error("Missing protocol parameters: '{0}'.")]
    MissingProtocolParameters(String),
}
