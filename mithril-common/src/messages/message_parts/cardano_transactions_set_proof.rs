use crate::entities::{HexEncodedKey, TransactionHash};
use serde::{Deserialize, Serialize};

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CardanoTransactionsSetProofMessagePart {
    /// Hashes of the certified transactions
    pub transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    pub proof: HexEncodedKey,
}
