use thiserror::Error;

/// Circuit-scoped errors for Halo2 STM validation and execution.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum CircuitError {
    /// Invalid relation parameters: quorum must be strictly lower than number of lotteries.
    #[error(
        "invalid circuit parameters: quorum ({quorum}) must be lower than num_lotteries ({num_lotteries})"
    )]
    InvalidCircuitParameters { quorum: u32, num_lotteries: u32 },

    /// Witness vector length does not match the configured quorum.
    #[error("witness length mismatch: expected quorum {expected_quorum}, got {actual}")]
    WitnessLengthMismatch {
        expected_quorum: usize,
        actual: usize,
    },

    /// Merkle sibling path length does not match the configured Merkle depth.
    #[error("merkle sibling length mismatch: expected depth {expected_depth}, got {actual}")]
    MerkleSiblingLengthMismatch {
        expected_depth: usize,
        actual: usize,
    },

    /// Merkle position path length does not match the configured Merkle depth.
    #[error("merkle position length mismatch: expected depth {expected_depth}, got {actual}")]
    MerklePositionLengthMismatch {
        expected_depth: usize,
        actual: usize,
    },

    /// The proving backend returned an error while executing this circuit.
    #[error("circuit execution failed in proving backend")]
    CircuitExecutionFailed,
}

impl CircuitError {
    /// Reconstruct typed circuit errors from the raw synthesis payload emitted
    /// by `StmCircuit::circuit(...)` before Midnight wraps it in `plonk::Error`.
    pub fn from_synthesis_message(message: &str) -> Option<Self> {
        parse_length_mismatch(
            message,
            "witness length mismatch: expected quorum ",
            ", got ",
        )
        .map(
            |(expected_quorum, actual)| CircuitError::WitnessLengthMismatch {
                expected_quorum,
                actual,
            },
        )
        .or_else(|| {
            parse_length_mismatch(
                message,
                "merkle sibling length mismatch: expected depth ",
                ", got ",
            )
            .map(|(expected_depth, actual)| {
                CircuitError::MerkleSiblingLengthMismatch {
                    expected_depth,
                    actual,
                }
            })
        })
        .or_else(|| {
            parse_length_mismatch(
                message,
                "merkle position length mismatch: expected depth ",
                ", got ",
            )
            .map(|(expected_depth, actual)| {
                CircuitError::MerklePositionLengthMismatch {
                    expected_depth,
                    actual,
                }
            })
        })
    }
}

/// Proving-side error categories for Halo2 STM.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ProvingError {
    /// Midnight setup panicked during key generation for this relation.
    #[error("midnight setup failed before proving")]
    MidnightSetupFailed,

    /// Circuit-level validation failed before or during proving.
    #[error(transparent)]
    Circuit(#[from] CircuitError),
}

/// Top-level proof-system errors grouped by proving vs verification phases.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum StmProofError {
    /// Proving-side failure with typed reason.
    #[error("proving failed: {0}")]
    ProvingFailed(#[from] ProvingError),

    /// Verification-side failure.
    #[error("verification failed")]
    VerificationFailed,
}

/// Result alias for Halo2 circuit-local operations.
pub type CircuitResult<T> = Result<T, CircuitError>;

/// Result alias for Halo2 proving/verification operations.
pub type StmProofResult<T> = Result<T, StmProofError>;

fn parse_length_mismatch(message: &str, prefix: &str, separator: &str) -> Option<(usize, usize)> {
    let remainder = message.strip_prefix(prefix)?;
    let (expected, actual) = remainder.split_once(separator)?;
    Some((expected.parse().ok()?, actual.parse().ok()?))
}
