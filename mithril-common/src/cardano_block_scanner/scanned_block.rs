use pallas_traverse::MultiEraBlock;

use crate::entities::{BlockHash, BlockNumber, CardanoTransaction, SlotNumber, TransactionHash};

/// A block scanned from a Cardano database
#[derive(Debug, Clone, PartialEq)]
pub struct ScannedBlock {
    /// Block hash
    pub block_hash: BlockHash,
    /// Block number
    pub block_number: BlockNumber,
    /// Slot number of the block
    pub slot_number: SlotNumber,
    /// Hashes of the transactions in the block
    pub transactions_hashes: Vec<TransactionHash>,
}

impl ScannedBlock {
    /// Scanned block factory
    pub fn new<T: Into<TransactionHash>, U: Into<BlockHash>>(
        block_hash: U,
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
            multi_era_block.hash().to_string(),
            BlockNumber(multi_era_block.number()),
            multi_era_block.slot(),
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
        self.transactions_hashes
            .into_iter()
            .map(|transaction_hash| {
                CardanoTransaction::new(
                    transaction_hash,
                    self.block_number,
                    self.slot_number,
                    self.block_hash.clone(),
                )
            })
            .collect::<Vec<_>>()
    }
}
