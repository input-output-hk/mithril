use crate::entities::TransactionHash;
use crate::messages::CardanoTransactionsSetProofMessagePart;
use serde::{Deserialize, Serialize};

/// A cryptographic proof for a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionsProofsMessage {
    /// Hash of the certificate that validate this proof merkle root
    pub certificate_hash: String,

    /// Transactions that have been certified
    pub certified_transactions: Vec<CardanoTransactionsSetProofMessagePart>,

    /// Transactions that could not be certified
    pub non_certified_transactions: Vec<TransactionHash>,
}

impl CardanoTransactionsProofsMessage {
    /// Create a new `CardanoTransactionsProofsMessage`
    pub fn new(
        certificate_hash: &str,
        certified_transactions: Vec<CardanoTransactionsSetProofMessagePart>,
        non_certified_transactions: Vec<TransactionHash>,
    ) -> Self {
        Self {
            certificate_hash: certificate_hash.to_string(),
            certified_transactions,
            non_certified_transactions,
        }
    }
}
