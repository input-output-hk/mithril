use crate::entities::{HexEncodedKey, TransactionHash};
use serde::{Deserialize, Serialize};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(target_family = "wasm", wasm_bindgen(getter_with_clone))]
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
            crate::entities::CardanoTransactionsSetProof::dummy().try_into().unwrap()
        }
    }
}
