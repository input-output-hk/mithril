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

    /// No certified items set proof to verify
    #[error("There's no certified {0} to verify")]
    NoCertifiedTransaction(&'static str),

    /// Not all certified items set proof have the same merkle root.
    ///
    /// This is problematic because all the set proof should be generated from the same
    /// merkle tree which root is signed in the [certificate][crate::entities::Certificate].
    #[error("All certified {0} set proofs must share the same Merkle root")]
    NonMatchingMerkleRoot(&'static str),

    /// An individual [crate::messages::MkSetProofMessagePart] could not be converted to a
    /// [crate::entities::MkSetProof] for verification.
    #[error("Malformed data or unknown {0} Set Proof format")]
    MalformedData(&'static str, #[source] StdError),
}
