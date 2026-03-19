use crate::SignerIndex;

use super::AggregateSignatureType;

/// Error types for aggregation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum AggregationError {
    /// Not enough signatures were collected, got this many instead.
    #[error("Not enough signatures. Got only {0} out of {1}.")]
    NotEnoughSignatures(u64, u64),

    #[error("Unsupported proof system: {0}")]
    UnsupportedProofSystem(AggregateSignatureType),

    /// There is a duplicate index
    #[error("Indices are not unique.")]
    IndexNotUnique,

    /// Signer registration data could not be found during witness assembly for SNARK prover input.
    #[error("Missing SNARK signer data for signer index {0}.")]
    MissingSnarkSignerData(SignerIndex),

    /// A signature selected for witness assembly is missing its SNARK component.
    #[error("Missing SNARK signature for lottery index {0}.")]
    MissingSnarkSignature(SignerIndex),
}

/// Errors which can be output by Mithril aggregate verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum AggregateSignatureError {
    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,

    /// Batch verification of STM aggregate signatures failed
    #[error("Batch verification of STM aggregate signatures failed")]
    BatchInvalid,

    /// The proof system used in the aggregate signature is not supported
    #[error("Unsupported proof system: {0}")]
    UnsupportedProofSystem(AggregateSignatureType),
}

/// Errors which can be outputted by the snark proof creation or verification.
#[cfg(feature = "future_snark")]
#[derive(Debug, Clone, thiserror::Error, PartialEq, Eq)]
pub enum SnarkError {
    #[error("The proof failed to verify.")]
    VerifyProofFail,
}
