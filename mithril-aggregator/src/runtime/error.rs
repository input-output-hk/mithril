use slog::{Logger, crit, error};
use thiserror::Error;

use mithril_common::StdError;

/// Error encountered or produced by the Runtime.
/// This enum represents the faith of the errors produced during the state
/// transitions.
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// Errors that need the runtime to try again without changing its state.
    #[error("An error occurred, runtime state kept. message = '{message}'")]
    KeepState {
        /// error message
        message: String,

        /// Eventual caught error
        #[source]
        nested_error: Option<StdError>,
    },
    /// A Critical error means the Runtime stops and the software exits with an
    /// error code.
    #[error("A critical error occurred, aborting runtime. message = '{message}'")]
    Critical {
        /// error message
        message: String,

        /// Eventual caught error
        #[source]
        nested_error: Option<StdError>,
    },
    /// An error that needs to re-initialize the state machine.
    #[error("An error occurred, runtime will be re-initialized. message = '{message}'")]
    ReInit {
        /// error message
        message: String,

        /// Eventual caught error
        #[source]
        nested_error: Option<StdError>,
    },
}

impl RuntimeError {
    /// Easy matching Critical errors.
    pub fn is_critical(&self) -> bool {
        matches!(self, RuntimeError::Critical { .. })
    }

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

    /// Write the error to the given logger.
    pub fn write_to_log(&self, logger: &Logger) {
        match self {
            Self::KeepState { nested_error, .. } | Self::ReInit { nested_error, .. } => {
                match nested_error {
                    None => error!(logger, "{self}"),
                    Some(err) => error!(logger, "{self}"; "nested_error" => ?err),
                }
            }
            Self::Critical { nested_error, .. } => match nested_error {
                None => crit!(logger, "{self}"),
                Some(err) => crit!(logger, "{self}"; "nested_error" => ?err),
            },
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

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use crate::test_tools::TestLogger;

    use super::*;

    fn nested_error_debug_string(error: &RuntimeError) -> String {
        let error = match error {
            RuntimeError::KeepState { nested_error, .. } => nested_error,
            RuntimeError::Critical { nested_error, .. } => nested_error,
            RuntimeError::ReInit { nested_error, .. } => nested_error,
        };
        match error {
            None => String::new(),
            Some(err) => {
                format!("{err:?}")
            }
        }
    }

    #[test]
    fn log_critical_without_nested_error() {
        let (logger, log_inspector) = TestLogger::memory();

        let error = RuntimeError::Critical {
            message: "Critical error".to_string(),
            nested_error: None,
        };
        error.write_to_log(&logger);

        assert!(log_inspector.contains_log(&format!("{error}")));
        assert!(!log_inspector.contains_log("nested_error"));
    }

    #[test]
    fn log_critical_with_nested_error() {
        let (logger, log_inspector) = TestLogger::memory();

        let error = RuntimeError::Critical {
            message: "Critical error".to_string(),
            nested_error: Some(
                anyhow!("Another context error")
                    .context("Context error")
                    .context("Critical nested error"),
            ),
        };
        error.write_to_log(&logger);

        assert!(log_inspector.contains_log(&format!("{error}")));
        assert!(log_inspector.contains_log(&nested_error_debug_string(&error)));
    }

    #[test]
    fn log_keep_state_without_nested_error() {
        let (logger, log_inspector) = TestLogger::memory();

        let error = RuntimeError::KeepState {
            message: "KeepState error".to_string(),
            nested_error: None,
        };
        error.write_to_log(&logger);

        assert!(log_inspector.contains_log(&format!("{error}")));
        assert!(!log_inspector.contains_log("nested_error"));
    }

    #[test]
    fn log_keep_state_with_nested_error() {
        let (logger, log_inspector) = TestLogger::memory();

        let error = RuntimeError::KeepState {
            message: "KeepState error".to_string(),
            nested_error: Some(
                anyhow!("Another context error")
                    .context("Context error")
                    .context("KeepState nested error"),
            ),
        };
        error.write_to_log(&logger);

        assert!(log_inspector.contains_log(&format!("{error}")));
        assert!(log_inspector.contains_log(&nested_error_debug_string(&error)));
    }

    #[test]
    fn log_reinit_without_nested_error() {
        let (logger, log_inspector) = TestLogger::memory();

        let error = RuntimeError::ReInit {
            message: "ReInit error".to_string(),
            nested_error: None,
        };
        error.write_to_log(&logger);

        assert!(log_inspector.contains_log(&format!("{error}")));
        assert!(!log_inspector.contains_log("nested_error"));
    }

    #[test]
    fn log_reinit_with_nested_error() {
        let (logger, log_inspector) = TestLogger::memory();

        let error = RuntimeError::ReInit {
            message: "ReInit error".to_string(),
            nested_error: Some(
                anyhow!("Another context error")
                    .context("Context error")
                    .context("ReInit nested error"),
            ),
        };
        error.write_to_log(&logger);

        assert!(log_inspector.contains_log(&format!("{error}")));
        assert!(log_inspector.contains_log(&nested_error_debug_string(&error)));
    }
}
