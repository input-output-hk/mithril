use thiserror::Error;

use crate::StdError;

/// Error encountered or produced by the verification of a set of proofs.
#[derive(Error, Debug)]
pub enum VerifyProofsV2Error {
    /// The verification of an individual [crate::messages::MkSetProofMessagePart] failed.
    #[error("Invalid set proof for {subject} hashes: {hashes:?}")]
    InvalidSetProof {
        /// Type of the item that failed the verification
        subject: &'static str,
        /// Hashes of the invalid items
        hashes: Vec<String>,
        /// Error source
        source: StdError,
    },

    /// No certified items to verify in the set proof
    #[error("There's no certified {0} to verify")]
    NoCertifiedItem(&'static str),

    /// An individual [crate::messages::MkSetProofMessagePart] could not be converted to a
    /// [crate::entities::MkSetProof] for verification.
    #[error("Malformed data or unknown {0} set proof format")]
    MalformedData(&'static str, #[source] StdError),
}
