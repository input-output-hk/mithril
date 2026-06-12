use thiserror::Error;

// TODO: remove this allow dead_code when IvcProof::verify is called from non-test code
#[allow(dead_code)]
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub(crate) enum IvcProofError {
    /// Transcript could not be prepared for KZG opening verification.
    #[error("IVC proof rejected: transcript preparation failed")]
    TranscriptPreparationFailed,

    /// Transcript contained unexpected trailing bytes after preparation.
    #[error("IVC proof rejected: transcript was not fully consumed")]
    TranscriptNotFullyConsumed,

    /// KZG opening equations (dual MSM check) did not verify.
    #[error("IVC proof rejected: KZG opening check failed")]
    KzgOpeningFailed,

    /// Folded accumulator pairing equation did not verify.
    #[error("IVC proof rejected: accumulator pairing check failed")]
    AccumulatorFailed,
}
