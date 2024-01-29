use crate::{crypto_helper::MKProof, entities::TransactionHash};

use serde::{Deserialize, Serialize};

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CardanoTransactionsSetProof {
    /// Hashes of the certified transactions
    transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    transactions_proof: MKProof,
}

impl CardanoTransactionsSetProof {
    /// CardanoTransactionsSetProof factory
    pub fn new(transactions_hashes: Vec<TransactionHash>, transactions_proof: MKProof) -> Self {
        Self {
            transactions_hashes,
            transactions_proof,
        }
    }
}

impl Default for CardanoTransactionsSetProof {
    fn default() -> Self {
        Self {
            transactions_hashes: Vec::new(),
            transactions_proof: todo!(),
        }
    }
}
