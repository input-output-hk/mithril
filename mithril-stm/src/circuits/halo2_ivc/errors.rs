use thiserror::Error;

/// Circuit-scoped errors for the IVC recursive SNARK circuit and its off-circuit helpers.
#[cfg_attr(not(test), allow(dead_code))]
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum IvcCircuitError {
    /// Off-circuit verifier rejected a certificate proof.
    #[error("Certificate proof verification rejected")]
    CertificateProofRejected,
}
