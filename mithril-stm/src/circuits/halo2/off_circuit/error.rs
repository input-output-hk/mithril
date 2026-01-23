use thiserror::Error;

// NOTE:
// At this stage of the Halo2 certificate prototype import, we keep
// `SignatureError` and `MerkleTreeError` as separate enums even though they
// currently share the same variants.
//
// This mirrors the structure of the original prototype and keeps error
// semantics explicit at call sites (signature vs merkle verification).
// As the circuit module evolves and error handling stabilizes, these error
// types may later be refactored or unified if appropriate.

#[derive(Debug, Error)]
pub enum SignatureError {
    #[error("Verification failed: Signature is invalid.")]
    VerificationFailed,
    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}

#[derive(Debug, Error)]
pub enum MerkleTreeError {
    #[error("Verification failed: Merkle proof is invalid.")]
    VerificationFailed,
    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}
