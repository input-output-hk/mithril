use crate::entities::{CardanoTransactionsSetProof, TransactionHash};
use serde::{Deserialize, Serialize};

/// A cryptographic proof for a set of Cardano transactions
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CardanoTransactionsProofsMessage {
    /// Transactions that have been certified
    certified_transactions: Vec<CardanoTransactionsSetProof>,

    /// Transactions that could not be certified
    non_certified_transactions: Vec<TransactionHash>,
}
