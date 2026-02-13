use thiserror::Error;

#[derive(Debug, Error)]
pub enum MerkleTreeError {
    #[error("Verification failed: Merkle proof is invalid.")]
    VerificationFailed,
    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}
