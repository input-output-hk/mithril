/// Error types related to merkle trees.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MerkleTreeError {
    /// Serialization error
    #[error("Serialization of a merkle tree failed")]
    SerializationError,

    #[cfg(feature = "future_snark")]
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    /// Invalid merkle path
    #[error("Path does not verify against root")]
    PathInvalid(Vec<u8>),

    /// Invalid merkle batch path
    #[error("Batch path does not verify against root")]
    BatchPathInvalid(Vec<u8>),
}
