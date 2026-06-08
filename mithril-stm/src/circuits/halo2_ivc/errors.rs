use midnight_proofs::plonk::Error as PlonkError;
use thiserror::Error;

/// Circuit-scoped errors for the IVC recursive SNARK circuit and its off-circuit helpers.
#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum IvcCircuitError {
    /// Off-circuit verifier rejected a certificate proof.
    #[error("Certificate proof verification rejected")]
    CertificateProofRejected,

    /// IVC verification key degree does not match the IVC circuit degree constant K.
    #[error(
        "IvcCircuitData::validate_ivc_verification_key_degree failed: expected k={expected}, got k={actual}"
    )]
    IvcVerificationKeyDegreeMismatch { expected: u32, actual: u32 },

    /// `assign_many` returned a different number of values than expected.
    #[error("IvcGadget: expected {expected} assigned values, got {actual}")]
    AssignedValueCountMismatch { expected: usize, actual: usize },

    /// Not enough advice columns were allocated to satisfy chip requirements.
    #[error(
        "IvcCircuitData::validate_column_counts failed: need {needed} advice columns, only {available} allocated"
    )]
    InsufficientAdviceColumns { needed: usize, available: usize },

    /// Not enough fixed columns were allocated to satisfy chip requirements.
    #[error(
        "IvcCircuitData::validate_column_counts failed: need {needed} fixed columns, only {available} allocated"
    )]
    InsufficientFixedColumns { needed: usize, available: usize },

    /// Off-circuit step transition: the certificate's epoch is neither equal to nor exactly
    /// one greater than the chain's current epoch.
    #[error(
        "IvcProverInput::prepare: certificate epoch {certificate_epoch} is not same-epoch or next-epoch of chain epoch {chain_epoch}"
    )]
    InvalidEpochTransition {
        certificate_epoch: u64,
        chain_epoch: u64,
    },

    /// Off-circuit step transition: the chain's step counter would overflow u64.
    #[error("IvcProverInput::prepare: step counter overflow advancing past {current}")]
    StepCounterOverflow { current: u64 },

    /// Off-circuit step transition: a same-epoch certificate's preimage announces a
    /// next-epoch lookahead that disagrees with the chain's previous state. All certs
    /// in the same epoch must encode the same next-epoch fields.
    #[error(
        "IvcProverInput::prepare: same-epoch cert at chain epoch {chain_epoch} announces \
         a next-epoch lookahead that does not match the chain state"
    )]
    SameEpochLookaheadMismatch { chain_epoch: u64 },
}

/// Convert an IVC circuit error into a Plonk synthesis error at gadget boundaries.
///
/// Takes `IvcCircuitError` directly rather than `StmError` (unlike the `halo2` variant) because
/// `halo2_ivc::synthesize` has no `anyhow` wrapping layer — call sites always hold the concrete
/// type and no downcasting is needed.
pub(crate) fn to_synthesis_error(error: IvcCircuitError) -> PlonkError {
    PlonkError::Synthesis(error.to_string())
}
