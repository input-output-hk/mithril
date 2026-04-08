use thiserror::Error;

pub mod schnorr_signature;
pub mod unique_signature;

#[derive(Debug, Error)]
pub enum SignatureError {
    #[error("Verification failed: Signature is invalid.")]
    VerificationFailed,
    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}
