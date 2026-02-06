use sqlite::Row;

use mithril_common::entities::{
    BlockHash, BlockNumber, CardanoBlockTransactionMkTreeNode, CardanoBlockWithTransactions,
    SlotNumber, TransactionHash,
};

use crate::database::Hydrator;
use crate::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Cardano block record is the representation of a cardano block stored in the sqlite database.
#[derive(Debug, PartialEq, Clone)]
pub struct CardanoBlockTransactionsRecord {
    /// Hash of the block
    pub block_hash: BlockHash,

    /// Number of the block
    pub block_number: BlockNumber,

    /// Slot number of the block
    pub slot_number: SlotNumber,

    /// List of transaction hashes included in the block
    pub transactions_hashes: Vec<TransactionHash>,
}

impl CardanoBlockTransactionsRecord {
    /// Factory
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
}

impl SqLiteEntity for CardanoBlockTransactionsRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let block_hash = row.read::<&str, _>(0);
        let block_number =
            Hydrator::try_to_u64("cardano_block.block_number", row.read::<i64, _>(1))?;
        let slot_number = Hydrator::try_to_u64("cardano_block.slot_number", row.read::<i64, _>(2))?;
        let transactions_hashes = row
            .read::<Option<&str>, _>(3)
            .map(|hashes| hashes.split(',').map(|s| s.to_string()).collect())
            .unwrap_or_default();

        Ok(Self {
            block_hash: block_hash.to_string(),
            block_number: BlockNumber(block_number),
            slot_number: SlotNumber(slot_number),
            transactions_hashes,
        })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            ("block_hash", "{:cardano_block:}.block_hash", "text"),
            ("block_number", "{:cardano_block:}.block_number", "int"),
            ("slot_number", "{:cardano_block:}.slot_number", "int"),
            (
                "transactions_hashes",
                "group_concat({:cardano_tx:}.transaction_hash, ',')",
                "text",
            ),
        ])
    }
}

impl From<CardanoBlockWithTransactions> for CardanoBlockTransactionsRecord {
    fn from(block: CardanoBlockWithTransactions) -> Self {
        Self::new(
            block.block_hash,
            block.block_number,
            block.slot_number,
            block.transactions_hashes,
        )
    }
}

impl CardanoBlockTransactionsRecord {
    /// Converts the block into a vector of [CardanoBlockTransactionMkTreeNode].
    pub fn into_mk_tree_node(self) -> Vec<CardanoBlockTransactionMkTreeNode> {
        let mut result = Vec::with_capacity(self.transactions_hashes.len() + 1);
        result.push(CardanoBlockTransactionMkTreeNode::Block {
            block_hash: self.block_hash.clone(),
            block_number: self.block_number,
            slot_number: self.slot_number,
        });
        result.extend(self.transactions_hashes.into_iter().map(|tx_hash| {
            CardanoBlockTransactionMkTreeNode::Transaction {
                transaction_hash: tx_hash,
                block_hash: self.block_hash.clone(),
                block_number: self.block_number,
                slot_number: self.slot_number,
            }
        }));

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_blocks_without_transactions_to_mk_tree_node() {
        assert_eq!(
            vec![CardanoBlockTransactionMkTreeNode::Block {
                block_hash: "block_hash-10".to_string(),
                block_number: BlockNumber(10),
                slot_number: SlotNumber(50),
            }],
            CardanoBlockTransactionsRecord::new(
                "block_hash-10",
                BlockNumber(10),
                SlotNumber(50),
                Vec::<&str>::new()
            )
            .into_mk_tree_node()
        );
    }

    #[test]
    fn convert_blocks_with_transactions_to_mk_tree_node() {
        assert_eq!(
            vec![
                CardanoBlockTransactionMkTreeNode::Block {
                    block_hash: "block_hash-10".to_string(),
                    block_number: BlockNumber(10),
                    slot_number: SlotNumber(50),
                },
                CardanoBlockTransactionMkTreeNode::Transaction {
                    transaction_hash: "tx-1".to_string(),
                    block_hash: "block_hash-10".to_string(),
                    block_number: BlockNumber(10),
                    slot_number: SlotNumber(50),
                },
                CardanoBlockTransactionMkTreeNode::Transaction {
                    transaction_hash: "tx-2".to_string(),
                    block_hash: "block_hash-10".to_string(),
                    block_number: BlockNumber(10),
                    slot_number: SlotNumber(50),
                },
            ],
            CardanoBlockTransactionsRecord::new(
                "block_hash-10",
                BlockNumber(10),
                SlotNumber(50),
                vec!["tx-1", "tx-2"]
            )
            .into_mk_tree_node()
        );
    }
}
