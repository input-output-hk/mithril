use pallas_traverse::MultiEraBlock;
use std::fmt::{Debug, Formatter};

use crate::entities::{BlockNumber, CardanoTransaction, ChainPoint, SlotNumber, TransactionHash};

/// A block scanned from a Cardano database
#[derive(Clone, PartialEq)]
pub struct ScannedBlock {
    /// Block hash
    pub block_hash: Vec<u8>,
    /// Block number
    pub block_number: BlockNumber,
    /// Slot number of the block
    pub slot_number: SlotNumber,
    /// Hashes of the transactions in the block
    pub transactions_hashes: Vec<TransactionHash>,
}

impl ScannedBlock {
    /// Scanned block factory
    pub fn new<B: Into<Vec<u8>>, T: Into<TransactionHash>>(
        block_hash: B,
        block_number: BlockNumber,
        slot_number: SlotNumber,
        transaction_hashes: Vec<T>,
    ) -> Self {
        Self {
            block_hash: block_hash.into(),
            block_number,
            slot_number,
            transactions_hashes: transaction_hashes.into_iter().map(|h| h.into()).collect(),
        }
    }

    pub(crate) fn convert(multi_era_block: MultiEraBlock) -> Self {
        let mut transactions = Vec::new();
        for tx in &multi_era_block.txs() {
            transactions.push(tx.hash().to_string());
        }

        Self::new(
            *multi_era_block.hash(),
            BlockNumber(multi_era_block.number()),
            SlotNumber(multi_era_block.slot()),
            transactions,
        )
    }

    /// Number of transactions in the block
    pub fn transactions_len(&self) -> usize {
        self.transactions_hashes.len()
    }

    /// Convert the scanned block into a list of Cardano transactions.
    ///
    /// Consume the block.
    pub fn into_transactions(self) -> Vec<CardanoTransaction> {
        let block_hash = hex::encode(&self.block_hash);
        self.transactions_hashes
            .into_iter()
            .map(|transaction_hash| {
                CardanoTransaction::new(
                    transaction_hash,
                    self.block_number,
                    self.slot_number,
                    block_hash.clone(),
                )
            })
            .collect::<Vec<_>>()
    }
}

impl Debug for ScannedBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("ScannedBlock");
        debug
            .field("block_hash", &hex::encode(&self.block_hash))
            .field("block_number", &self.block_number)
            .field("slot_number", &self.slot_number)
            .field("transactions_hashes", &self.transactions_hashes)
            .finish()
    }
}

impl From<&ScannedBlock> for ChainPoint {
    fn from(scanned_block: &ScannedBlock) -> Self {
        ChainPoint::new(
            scanned_block.slot_number,
            scanned_block.block_number,
            hex::encode(&scanned_block.block_hash),
        )
    }
}
