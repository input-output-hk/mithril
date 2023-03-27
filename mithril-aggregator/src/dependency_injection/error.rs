//! Module dedicated to dependencies builder error management.

use std::{error::Error, fmt::Display};

use config::ConfigError;
use mithril_common::StdError;

/// Result with the [DependencyBuilderError] error.
pub type Result<T> = std::result::Result<T, DependenciesBuilderError>;

/// Error that can occure during dependencies initialization process.
#[derive(Debug)]
pub enum DependenciesBuilderError {
    /// Unrecoverable system initialization failure
    Initialization {
        /// Error context message
        message: String,

        /// Eventual nested error
        error: Option<StdError>,
    },

    /// Configuration parameter missing for initialization.
    MissingConfiguration(String),
}

impl Display for DependenciesBuilderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Initialization { message, error } => {
                if let Some(nested) = error {
                    write!(f, "Dependency initialization error: «{message}» with additional nested error: '{nested}'.")
                } else {
                    write!(f, "Dependency initialization error: «{message}».")
                }
            }
            Self::MissingConfiguration(field) => {
                write!(f, "Missing configuration setting '{field}'.")
            }
        }
    }
}

impl Error for DependenciesBuilderError {}

impl From<StdError> for DependenciesBuilderError {
    fn from(value: StdError) -> Self {
        DependenciesBuilderError::Initialization {
            message: "subsystem error".to_string(),
            error: Some(value),
        }
    }
}

impl From<ConfigError> for DependenciesBuilderError {
    fn from(value: ConfigError) -> Self {
        Self::MissingConfiguration(format!("{value}"))
    }
}
