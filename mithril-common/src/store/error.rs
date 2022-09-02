use thiserror::Error;

use super::adapter::AdapterError;

/// Generic error type for stores.
#[derive(Error, Debug)]
pub enum StoreError {
    /// Error raised when the underlying [adapter][StoreAdapter] raise an error.
    #[error("Store adapter raised an error: {0}")]
    AdapterError(#[from] AdapterError),
}
