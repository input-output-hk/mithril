use thiserror::Error;

use crate::crypto_helper::KesPeriod;

/// KES verification error
#[derive(Error, Debug)]
pub enum KesVerifyError {
    /// Error raised when an operational certificate is invalid
    #[error("invalid operational certificate")]
    OpCertInvalid,

    /// Error raised when a KES Signature verification fails
    #[error("KES signature verification error: CurrentKesPeriod={0}, StartKesPeriod={1}")]
    SignatureInvalid(KesPeriod, KesPeriod),
}

/// KES signature error
#[derive(Error, Debug, PartialEq, Eq)]
pub enum KesSignError {
    /// Error raised when a KES update error occurs
    #[error("KES key cannot be updated for evolution {0}")]
    UpdateKey(KesPeriod),

    /// Period of key file does not match with period provided by user
    #[error("Period of key file {0} does not match with period provided by user {1}")]
    PeriodMismatch(KesPeriod, KesPeriod),
}
