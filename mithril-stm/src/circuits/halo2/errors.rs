use thiserror::Error;

/// Circuit-scoped errors for Halo2 STM validation and execution.
#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum CircuitError {
    /// Invalid relation parameters: quorum must be strictly lower than number of lotteries.
    #[error(
        "invalid circuit parameters: quorum ({quorum}) must be lower than num_lotteries ({num_lotteries})"
    )]
    InvalidCircuitParameters { quorum: u32, num_lotteries: u32 },

    /// Witness vector length does not match the configured quorum.
    #[error("witness length mismatch: expected quorum {expected_quorum}, got {actual}")]
    WitnessLengthMismatch { expected_quorum: u32, actual: u32 },

    /// Merkle sibling path length does not match the configured Merkle depth.
    #[error("merkle sibling length mismatch: expected depth {expected_depth}, got {actual}")]
    MerkleSiblingLengthMismatch { expected_depth: u32, actual: u32 },

    /// Merkle position path length does not match the configured Merkle depth.
    #[error("merkle position length mismatch: expected depth {expected_depth}, got {actual}")]
    MerklePositionLengthMismatch { expected_depth: u32, actual: u32 },

    /// The proving backend returned an error while executing this circuit.
    #[error("circuit execution failed in proving backend: {0}")]
    CircuitExecutionFailed(String),
}

impl CircuitError {
    /// Reconstruct typed circuit errors from the raw synthesis payload emitted
    /// by `StmCircuit::circuit(...)` before Midnight wraps it in `plonk::Error`.
    ///
    /// This is a workaround: Midnight currently erases typed circuit errors and
    /// returns only `Error::Synthesis(String)`. We re-hydrate known validation failures
    /// from message patterns. This may change when upstream error typing changes.
    #[cfg_attr(not(test), allow(dead_code))]
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
#[cfg_attr(not(test), allow(dead_code))]
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
#[cfg_attr(not(test), allow(dead_code))]
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
#[cfg_attr(not(test), allow(dead_code))]
pub type StmProofResult<T> = Result<T, StmProofError>;

#[cfg_attr(not(test), allow(dead_code))]
fn parse_length_mismatch(message: &str, prefix: &str, separator: &str) -> Option<(u32, u32)> {
    let remainder = message.strip_prefix(prefix)?;
    let (expected, actual) = remainder.split_once(separator)?;
    Some((expected.parse().ok()?, actual.parse().ok()?))
}

#[cfg(test)]
mod tests {
    use super::CircuitError;

    #[test]
    fn round_trip_witness_length_mismatch() {
        let error = CircuitError::WitnessLengthMismatch {
            expected_quorum: 3,
            actual: 2,
        };
        let message = error.to_string();
        assert_eq!(Some(error), CircuitError::from_synthesis_message(&message));
    }

    #[test]
    fn round_trip_merkle_sibling_length_mismatch() {
        let error = CircuitError::MerkleSiblingLengthMismatch {
            expected_depth: 10,
            actual: 9,
        };
        let message = error.to_string();
        assert_eq!(Some(error), CircuitError::from_synthesis_message(&message));
    }

    #[test]
    fn round_trip_merkle_position_length_mismatch() {
        let error = CircuitError::MerklePositionLengthMismatch {
            expected_depth: 10,
            actual: 11,
        };
        let message = error.to_string();
        assert_eq!(Some(error), CircuitError::from_synthesis_message(&message));
    }

    #[test]
    fn invalid_circuit_parameters_is_not_rehydrated_from_synthesis_message() {
        let message = CircuitError::InvalidCircuitParameters {
            quorum: 3,
            num_lotteries: 3,
        }
        .to_string();
        assert_eq!(None, CircuitError::from_synthesis_message(&message));
    }

    #[test]
    fn unknown_synthesis_message_is_not_rehydrated() {
        assert_eq!(
            None,
            CircuitError::from_synthesis_message("unknown backend synthesis failure")
        );
    }
}
