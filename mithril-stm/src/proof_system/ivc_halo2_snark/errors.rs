//! Errors returned by the IVC SNARK proof system.

use std::path::PathBuf;

use thiserror::Error;

/// Errors returned by the IVC SNARK proof system.
#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum IvcSnarkError {
    /// Failed to open the rolling state file for reading.
    #[error("Failed to open rolling state file at {0:?}")]
    RollingStateOpen(Box<PathBuf>),

    /// Failed to create the rolling state file for writing.
    #[error("Failed to create rolling state file at {0:?}")]
    RollingStateCreate(Box<PathBuf>),

    /// Failed to save the rolling state to the file.
    #[error("Failed to save rolling state to file")]
    RollingStateSave,

    /// Failed to read the rolling state from the file.
    #[error("Failed to read rolling state from file")]
    RollingStateRead,
}
