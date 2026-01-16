#[cfg(feature = "future_snark")]
use super::{PrimeOrderProjectivePoint, UniqueSchnorrSignature};

/// Error types for the Unique Schnorr signatures.
#[cfg(feature = "future_snark")]
#[derive(Debug, thiserror::Error, Eq, PartialEq)]
pub enum UniqueSchnorrSignatureError {
    /// Invalid Single signature
    #[error("Invalid Unique Schnorr single signature")]
    SignatureInvalid(Box<UniqueSchnorrSignature>),

    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    Serialization,

    /// This error occurs when the serialization of the scalar field bytes failed
    #[error("Invalid scalar field element bytes")]
    ScalarFieldElementSerialization,

    /// This error occurs when the serialization of the base field bytes failed
    #[error("Invalid base field element bytes")]
    BaseFieldElementSerialization,

    /// This error occurs when the serialization of the projective point bytes failed
    #[error("Invalid projective point bytes")]
    ProjectivePointSerialization,

    /// This error occurs when the serialization of the prime order projective point bytes failed
    #[error("Invalid prime order projective point bytes")]
    PrimeOrderProjectivePointSerialization,

    /// This error occurs when the random scalar fails to generate during the signature
    #[error("Failed generation of the signature's random scalar")]
    RandomScalarGeneration,

    /// This error occurs when signing key is zero or one.
    #[error("The signing key is invalid.")]
    InvalidSigningKey,

    /// Given point is not on the curve
    #[error("Given point is not on the curve")]
    PointIsNotOnCurve(Box<PrimeOrderProjectivePoint>),

    /// Given point is not prime order
    #[error("Given point is not prime order")]
    PointIsNotPrimeOrder(Box<PrimeOrderProjectivePoint>),
}
