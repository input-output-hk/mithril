use serde::{Deserialize, Serialize};

use crate::entities::{
    BlockHash, BlockNumber, CardanoBlock, CardanoTransaction, IntoMKTreeNode, MkSetProof,
    SlotNumber,
};
use crate::{
    StdError, StdResult,
    crypto_helper::ProtocolMkProof,
    entities::{HexEncodedKey, TransactionHash},
};

#[cfg(target_family = "wasm")]
use wasm_bindgen::prelude::*;

/// A cryptographic proof of a set of items is included in a merkle tree
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[cfg_attr(target_family = "wasm", wasm_bindgen(getter_with_clone))]
pub struct MkSetProofMessagePart<T> {
    /// Hashes of the certified transactions
    pub items: Vec<T>,

    /// Proof of the transactions
    pub proof: HexEncodedKey,
}

impl<T: Clone> MkSetProofMessagePart<T> {
    /// Verify this proof by converting to `MkSetProof<U>` and validating
    pub fn verify<U>(&self) -> StdResult<MkSetProof<U>>
    where
        U: IntoMKTreeNode + Clone + From<T>,
    {
        let mk_set_proof: MkSetProof<U> = self.clone().try_into()?;
        mk_set_proof.verify()?;
        Ok(mk_set_proof)
    }
}

impl<T, U: Into<T> + IntoMKTreeNode + Clone> TryFrom<MkSetProof<U>> for MkSetProofMessagePart<T> {
    type Error = StdError;

    fn try_from(value: MkSetProof<U>) -> Result<Self, Self::Error> {
        Ok(Self {
            items: value.items.into_iter().map(Into::into).collect(),
            proof: value.proof.to_json_hex()?,
        })
    }
}

impl<T: IntoMKTreeNode + Clone, U: Into<T>> TryFrom<MkSetProofMessagePart<U>> for MkSetProof<T> {
    type Error = StdError;

    fn try_from(value: MkSetProofMessagePart<U>) -> Result<Self, Self::Error> {
        Ok(Self {
            items: value.items.into_iter().map(Into::into).collect(),
            proof: ProtocolMkProof::from_json_hex(&value.proof)?,
        })
    }
}

/// Cardano block message representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CardanoBlockMessagePart {
    /// Block hash
    #[cfg_attr(target_family = "wasm", wasm_bindgen(getter_with_clone))]
    pub block_hash: BlockHash,
    /// Block number
    pub block_number: BlockNumber,
    /// Slot number of the block
    pub slot_number: SlotNumber,
}

impl CardanoBlockMessagePart {
    /// CardanoBlockMessagePart factory
    pub fn new<U: Into<BlockHash>>(
        block_hash: U,
        block_number: BlockNumber,
        slot_number: SlotNumber,
    ) -> Self {
        Self {
            block_hash: block_hash.into(),
            block_number,
            slot_number,
        }
    }
}

impl From<CardanoBlock> for CardanoBlockMessagePart {
    fn from(block: CardanoBlock) -> Self {
        Self {
            block_hash: block.block_hash,
            block_number: block.block_number,
            slot_number: block.slot_number,
        }
    }
}

impl From<CardanoBlockMessagePart> for CardanoBlock {
    fn from(block: CardanoBlockMessagePart) -> Self {
        Self {
            block_hash: block.block_hash,
            block_number: block.block_number,
            slot_number: block.slot_number,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
/// Cardano transaction message representation
pub struct CardanoTransactionMessagePart {
    /// Unique hash of the transaction
    #[cfg_attr(target_family = "wasm", wasm_bindgen(getter_with_clone))]
    pub transaction_hash: TransactionHash,

    /// Block number of the transaction
    pub block_number: BlockNumber,

    /// Slot number of the transaction
    pub slot_number: SlotNumber,

    /// Block hash of the transaction
    #[cfg_attr(target_family = "wasm", wasm_bindgen(getter_with_clone))]
    pub block_hash: BlockHash,
}

impl CardanoTransactionMessagePart {
    /// CardanoTransactionMessagePart factory
    pub fn new<T: Into<TransactionHash>, U: Into<BlockHash>>(
        hash: T,
        block_number: BlockNumber,
        slot_number: SlotNumber,
        block_hash: U,
    ) -> Self {
        Self {
            transaction_hash: hash.into(),
            block_number,
            slot_number,
            block_hash: block_hash.into(),
        }
    }
}

impl From<CardanoTransaction> for CardanoTransactionMessagePart {
    fn from(transaction: CardanoTransaction) -> Self {
        Self {
            transaction_hash: transaction.transaction_hash,
            block_number: transaction.block_number,
            slot_number: transaction.slot_number,
            block_hash: transaction.block_hash,
        }
    }
}

impl From<CardanoTransactionMessagePart> for CardanoTransaction {
    fn from(transaction: CardanoTransactionMessagePart) -> Self {
        Self {
            transaction_hash: transaction.transaction_hash,
            block_number: transaction.block_number,
            slot_number: transaction.slot_number,
            block_hash: transaction.block_hash,
        }
    }
}
