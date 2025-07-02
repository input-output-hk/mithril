use slog::{Logger, crit, error};
use thiserror::Error;

use mithril_common::StdError;
use mithril_common::entities::EpochError;

use crate::RunnerError;

/// RuntimeError
/// Error kinds tied to their faith in the state machine.
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// KeepState error means the runtime will keep its state and try to cycle
    /// again.
    #[error("An error occurred, runtime state kept. message = '{message}'")]
    KeepState {
        /// Context error message
        message: String,

        /// Eventual previous error message
        #[source]
        nested_error: Option<StdError>,
    },
    /// Critical error means the runtime will exit and the software will return
    /// an error code.
    #[error("A critical error occurred, aborting runtime. message = '{message}'")]
    Critical {
        /// Context error message
        message: String,

        /// Eventual previous error message
        #[source]
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

    /// Write the error to the given logger.
    pub fn write_to_log(&self, logger: &Logger) {
        match self {
            Self::KeepState { nested_error, .. } => match nested_error {
                None => error!(logger, "{self}"),
                Some(err) => error!(logger, "{self}"; "nested_error" => ?err),
            },
            Self::Critical { nested_error, .. } => match nested_error {
                None => crit!(logger, "{self}"),
                Some(err) => crit!(logger, "{self}"; "nested_error" => ?err),
            },
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

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use crate::test_tools::TestLogger;

    use super::*;

    fn nested_error_debug_string(error: &RuntimeError) -> String {
        let error = match error {
            RuntimeError::KeepState { nested_error, .. } => nested_error,
            RuntimeError::Critical { nested_error, .. } => nested_error,
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
}
