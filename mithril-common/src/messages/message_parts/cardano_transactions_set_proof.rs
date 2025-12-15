use serde::{Deserialize, Serialize};
#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

use crate::{
    StdError,
    crypto_helper::ProtocolMkProof,
    entities::{CardanoTransactionsSetProof, HexEncodedKey, TransactionHash},
};

/// A cryptographic proof of a set of Cardano transactions is included in the global Cardano transactions set
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(target_family = "wasm", wasm_bindgen(getter_with_clone))]
pub struct CardanoTransactionsSetProofMessagePart {
    /// Hashes of the certified transactions
    pub transactions_hashes: Vec<TransactionHash>,

    /// Proof of the transactions
    pub proof: HexEncodedKey,
}

impl TryFrom<CardanoTransactionsSetProof> for CardanoTransactionsSetProofMessagePart {
    type Error = StdError;

    fn try_from(proof: CardanoTransactionsSetProof) -> Result<Self, Self::Error> {
        Ok(Self {
            transactions_hashes: proof.transactions_hashes,
            proof: proof.transactions_proof.to_json_hex()?,
        })
    }
}

impl TryFrom<CardanoTransactionsSetProofMessagePart> for CardanoTransactionsSetProof {
    type Error = StdError;

    fn try_from(proof: CardanoTransactionsSetProofMessagePart) -> Result<Self, Self::Error> {
        Ok(Self {
            transactions_hashes: proof.transactions_hashes,
            transactions_proof: ProtocolMkProof::from_json_hex(&proof.proof)?,
        })
    }
}
