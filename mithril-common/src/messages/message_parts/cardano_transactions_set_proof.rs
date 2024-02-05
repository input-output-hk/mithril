use crate::entities::{CardanoTransactionsSetProof, HexEncodedKey, TransactionHash};
use serde::{Deserialize, Serialize};

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CardanoTransactionsSetProofMessagePart {
    /// Hashes of the certified transactions
    pub transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    pub proof: HexEncodedKey,
}

impl CardanoTransactionsSetProofMessagePart {
    cfg_test_tools! {
        /// Retrieve a dummy proof (for test only)
        pub fn dummy() -> Self {
            CardanoTransactionsSetProof::dummy().try_into().unwrap()
        }
    }
}
