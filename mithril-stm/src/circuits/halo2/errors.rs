use thiserror::Error;

/// Circuit-scoped errors for Halo2 STM validation and execution.
#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum StmCircuitError {
    /// Invalid relation parameters: quorum must be strictly lower than number of lotteries.
    #[error(
        "Circuit::validate_parameters failed: quorum ({quorum}) must be lower than num_lotteries ({num_lotteries})"
    )]
    InvalidCircuitParameters { quorum: u32, num_lotteries: u32 },

    /// Witness vector length does not match the configured quorum.
    #[error(
        "Circuit::validate_witness_length failed: expected quorum {expected_quorum}, got {actual}"
    )]
    WitnessLengthMismatch { expected_quorum: u32, actual: u32 },

    /// Merkle sibling path length does not match the configured Merkle depth.
    #[error(
        "Circuit::validate_merkle_sibling_length failed: expected depth {expected_depth}, got {actual}"
    )]
    MerkleSiblingLengthMismatch { expected_depth: u32, actual: u32 },

    /// Merkle position path length does not match the configured Merkle depth.
    #[error(
        "Circuit::validate_merkle_position_length failed: expected depth {expected_depth}, got {actual}"
    )]
    MerklePositionLengthMismatch { expected_depth: u32, actual: u32 },

    /// Proof was generated but rejected by the verifier.
    #[error("Proof verification rejected")]
    VerificationRejected,
}
