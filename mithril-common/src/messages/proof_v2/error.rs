use thiserror::Error;

use crate::StdError;
use crate::entities::TransactionHash;

/// Error encountered or produced by the [cardano transaction proof verification][ProofsV2CardanoTransactionsMessage::verify].
#[derive(Error, Debug)]
pub enum VerifyProofsV2Error {
    /// The verification of an individual [CardanoTransactionsSetProofMessagePart] failed.
    #[error("Invalid set proof for transactions hashes: {transactions_hashes:?}")]
    InvalidSetProof {
        /// Hashes of the invalid transactions
        transactions_hashes: Vec<TransactionHash>,
        /// Error source
        source: StdError,
    },

    /// No certified transactions set proof to verify
    #[error("There's no certified transaction to verify")]
    NoCertifiedTransaction,

    /// Not all certified transactions set proof have the same merkle root.
    ///
    /// This is problematic because all the set proof should be generated from the same
    /// merkle tree which root is signed in the [certificate][crate::entities::Certificate].
    #[error("All certified transactions set proofs must share the same Merkle root")]
    NonMatchingMerkleRoot,

    /// An individual [CardanoTransactionsSetProofMessagePart] could not be converted to a
    /// [ProofsV2CardanoTransactionsMessage] for verification.
    #[error("Malformed data or unknown Cardano Set Proof format")]
    MalformedData(#[source] StdError),
}
