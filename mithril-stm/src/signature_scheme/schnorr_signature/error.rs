#[cfg(feature = "future_snark")]
use super::{SchnorrSignature, SchnorrVerificationKey};

/// Error types for Schnorr signatures.
#[cfg(feature = "future_snark")]
#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum SchnorrSignatureError {
    /// Invalid Single signature
    #[error("Invalid Schnorr single signature")]
    SignatureInvalid(Box<SchnorrSignature>),

    /// Invalid Verification key
    #[error("Invalid Schnorr Verification key")]
    VerificationKeyInvalid(Box<SchnorrVerificationKey>),

    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,

    /// This error occurs when the signing key fails to generate
    #[error("Failed generation of the signing key")]
    SigningKeyGenerationError,

    /// This error occurs when the random scalar fails to generate during the signature
    #[error("Failed generation of the signature's random scalar")]
    RandomScalarGenerationError,
}
