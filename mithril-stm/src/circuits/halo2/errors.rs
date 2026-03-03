use midnight_proofs::plonk::Error as PlonkError;
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

    /// The proving backend returned an error while executing this circuit.
    #[error("Circuit execution failed in proving backend: {0}")]
    CircuitExecutionFailed(String),
}

/// Result alias for Halo2 circuit-local operations.
pub type StmCircuitResult<T> = Result<T, StmCircuitError>;

/// Boundary adapter required by Midnight's circuit API.
///
/// The circuit relation must return `plonk::Error`, so typed `StmCircuitError` values are
/// converted only at that boundary.
impl From<StmCircuitError> for PlonkError {
    fn from(error: StmCircuitError) -> Self {
        PlonkError::Synthesis(error.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::{PlonkError, StmCircuitError};

    #[test]
    fn conversion_to_synthesis_error_preserves_message_for_all_variants() {
        let test_cases = vec![
            StmCircuitError::InvalidCircuitParameters {
                quorum: 3,
                num_lotteries: 3,
            },
            StmCircuitError::WitnessLengthMismatch {
                expected_quorum: 3,
                actual: 2,
            },
            StmCircuitError::MerkleSiblingLengthMismatch {
                expected_depth: 10,
                actual: 9,
            },
            StmCircuitError::MerklePositionLengthMismatch {
                expected_depth: 10,
                actual: 11,
            },
            StmCircuitError::CircuitExecutionFailed("backend failure".to_string()),
        ];

        for circuit_error in test_cases {
            let plonk_error = PlonkError::from(circuit_error.clone());
            match plonk_error {
                PlonkError::Synthesis(message) => assert_eq!(message, circuit_error.to_string()),
                other => panic!("expected synthesis error, got: {other:?}"),
            }
        }
    }
}
