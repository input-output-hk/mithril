use crate::entities::{BlockHash, BlockNumber, CardanoTransaction, SlotNumber, TransactionHash};

/// Cardano block representation
#[derive(Debug, Clone, PartialEq)]
pub struct CardanoBlock {
    /// Block hash
    pub block_hash: BlockHash,
    /// Block number
    pub block_number: BlockNumber,
    /// Slot number of the block
    pub slot_number: SlotNumber,
}

impl CardanoBlock {
    /// CardanoBlock factory
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

/// Cardano block representation, including the hashes of the transactions in the block
#[derive(Debug, Clone, PartialEq)]
pub struct CardanoBlockWithTransactions {
    /// Block hash
    pub block_hash: BlockHash,
    /// Block number
    pub block_number: BlockNumber,
    /// Slot number of the block
    pub slot_number: SlotNumber,
    /// Hashes of the transactions in the block
    pub transactions_hashes: Vec<TransactionHash>,
}

impl CardanoBlockWithTransactions {
    /// CardanoBlockWithTransactions factory
    pub fn new<U: Into<BlockHash>, T: Into<TransactionHash>>(
        block_hash: U,
        block_number: BlockNumber,
        slot_number: SlotNumber,
        tx_hashes: Vec<T>,
    ) -> Self {
        Self {
            block_hash: block_hash.into(),
            block_number,
            slot_number,
            transactions_hashes: tx_hashes.into_iter().map(Into::into).collect(),
        }
    }

    /// Converts the block into a vector of transactions.
    pub fn into_transactions(self) -> Vec<CardanoTransaction> {
        self.transactions_hashes
            .into_iter()
            .map(|tx_hash| {
                CardanoTransaction::new(
                    tx_hash,
                    self.block_number,
                    self.slot_number,
                    self.block_hash.clone(),
                )
            })
            .collect()
    }

    /// Returns the number of transactions in the block.
    pub fn transactions_count(&self) -> usize {
        self.transactions_hashes.len()
    }
}
