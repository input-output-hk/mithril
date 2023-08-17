use std::fmt::Display;

use mithril_common::entities::EpochError;
use mithril_common::StdError;

use crate::RunnerError;

/// RuntimeError
/// Error kinds tied to their faith in the state machine.
#[derive(Debug)]
pub enum RuntimeError {
    /// KeepState error means the runtime will keep its state and try to cycle
    /// again.
    KeepState {
        /// Context error message
        message: String,

        /// Eventual previous error message
        nested_error: Option<StdError>,
    },
    /// Critical error means the runtime will exit and the software will return
    /// an error code.
    Critical {
        /// Context error message
        message: String,

        /// Eventual previous error message
        nested_error: Option<StdError>,
    },
}

impl RuntimeError {
    /// Easy matching Critical errors.
    pub fn is_critical(&self) -> bool {
        matches!(
            self,
            RuntimeError::Critical {
                message: _,
                nested_error: _
            }
        )
    }
}

impl std::error::Error for RuntimeError {}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::KeepState {
                message,
                nested_error,
            } => {
                let nested = if let Some(error) = nested_error {
                    format!("nested error = {error:?}")
                } else {
                    String::new()
                };
                write!(
                    f,
                    "An error occured, runtime state kept. message = '{message}'. {nested}"
                )
            }
            Self::Critical {
                message,
                nested_error,
            } => {
                let nested = if let Some(error) = nested_error {
                    format!("nested error = {error:?}")
                } else {
                    String::new()
                };
                write!(
                    f,
                    "A critical error occured, aborting runtime. message = '{message}'. {nested}"
                )
            }
        }
    }
}

impl From<RunnerError> for RuntimeError {
    fn from(value: RunnerError) -> Self {
        Self::KeepState {
            message: "runner failed".to_string(),
            nested_error: Some(value.into()),
        }
    }
}

impl From<EpochError> for RuntimeError {
    fn from(value: EpochError) -> Self {
        Self::KeepState {
            message: "Epoch offset conversion failed".to_string(),
            nested_error: Some(value.into()),
        }
    }
}
