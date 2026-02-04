use std::cmp::Ordering;

use crate::crypto_helper::MKTreeNode;
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

/// Leaf of the Merkle tree representing blocks and transactions
///
/// When ordering in collections:
/// - all blocks are first, then all transactions
/// - blocks are ordered by block number, then slot number, then block hash
/// - transactions are ordered by block number, then slot number, then block hash, then transaction hash
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CardanoBlockTransactionMkTreeNode {
    /// Leaf representing a block
    Block {
        /// Block hash
        block_hash: BlockHash,
        /// Block number
        block_number: BlockNumber,
        /// Slot number of the block
        slot_number: SlotNumber,
    },
    /// Leaf representing a transaction
    Transaction {
        /// Unique hash of the transaction
        transaction_hash: TransactionHash,
        /// Block number of the transaction
        block_number: BlockNumber,
        /// Slot number of the transaction
        slot_number: SlotNumber,
        /// Block hash of the transaction
        block_hash: BlockHash,
    },
}

impl CardanoBlockTransactionMkTreeNode {
    fn leaf_identifier(&self) -> Vec<u8> {
        match self {
            Self::Block {
                block_hash,
                block_number,
                slot_number,
            } => format!("Block/{block_hash}/{block_number}/{slot_number}").into_bytes(),
            Self::Transaction {
                transaction_hash,
                block_hash,
                block_number,
                slot_number,
            } => format!("Tx/{transaction_hash}/{block_hash}/{block_number}/{slot_number}",)
                .into_bytes(),
        }
    }
}

impl From<CardanoBlockTransactionMkTreeNode> for MKTreeNode {
    fn from(value: CardanoBlockTransactionMkTreeNode) -> Self {
        MKTreeNode::new(value.leaf_identifier())
    }
}

impl PartialOrd for CardanoBlockTransactionMkTreeNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl Ord for CardanoBlockTransactionMkTreeNode {
    fn cmp(&self, other: &Self) -> Ordering {
        use CardanoBlockTransactionMkTreeNode::{Block, Transaction};

        match (self, other) {
            (Block { .. }, Transaction { .. }) => Ordering::Less,
            (Transaction { .. }, Block { .. }) => Ordering::Greater,
            (
                Block {
                    block_number,
                    block_hash,
                    slot_number,
                },
                Block {
                    block_number: other_block_number,
                    block_hash: other_block_hash,
                    slot_number: other_slot_number,
                },
            ) => block_number
                .cmp(other_block_number)
                .then(slot_number.cmp(other_slot_number))
                .then(block_hash.cmp(other_block_hash)),
            (
                Transaction {
                    block_number,
                    slot_number,
                    block_hash,
                    transaction_hash,
                },
                Transaction {
                    block_number: other_block_number,
                    slot_number: other_slot_number,
                    block_hash: other_block_hash,
                    transaction_hash: other_transaction_hash,
                },
            ) => block_number
                .cmp(other_block_number)
                .then(slot_number.cmp(other_slot_number))
                .then(block_hash.cmp(other_block_hash))
                .then(transaction_hash.cmp(other_transaction_hash)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! block_node {
        (num: $block_number:expr, slot: $slot_number:expr) => {
            block_node!(hash:format!("block_hash-{}", $block_number), num: $block_number, slot: $slot_number)
        };
        (hash: $block_hash:expr, num: $block_number:expr, slot: $slot_number:expr) => {
            CardanoBlockTransactionMkTreeNode::Block {
                block_hash: $block_hash.to_string(),
                block_number: BlockNumber($block_number),
                slot_number: SlotNumber($slot_number),
            }
        };
    }

    macro_rules! tx_node {
        (hash: $tx_hash:expr, block: $block_number:expr, slot: $slot_number:expr) => {
            tx_node!(hash: $tx_hash, block_hash: format!("block_hash-{}", $tx_hash), block: $block_number, slot: $slot_number)
        };
        (hash: $tx_hash:expr, block_hash: $block_hash:expr, block: $block_number:expr, slot: $slot_number:expr) => {
            CardanoBlockTransactionMkTreeNode::Transaction {
                transaction_hash: $tx_hash.to_string(),
                block_hash: $block_hash.to_string(),
                block_number: BlockNumber($block_number),
                slot_number: SlotNumber($slot_number),
            }
        };
    }

    #[test]
    fn block_node_leaf_identifier() {
        // expected: "Block"/BlockHash/BlockNumber/SlotNumber
        assert_eq!(
            "Block/block_hash-5/5/6".to_string().into_bytes(),
            block_node!(hash: "block_hash-5", num: 5, slot: 6).leaf_identifier()
        );
        assert_eq!(
            "Block/block_hash-10/9/13".to_string().into_bytes(),
            block_node!(hash: "block_hash-10", num: 9, slot: 13).leaf_identifier()
        );
    }

    #[test]
    fn transaction_node_leaf_identifier() {
        // expected: "Tx"/TransactionHash/BlockHash/BlockNumber/SlotNumber
        assert_eq!(
            "Tx/tx_hash-5/block_hash-5/5/6".to_string().into_bytes(),
            tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 6)
                .leaf_identifier()
        );
        assert_eq!(
            "Tx/tx_hash-10/block_hash-10/9/13".to_string().into_bytes(),
            tx_node!(hash: "tx_hash-10", block_hash: "block_hash-10", block: 9, slot: 13)
                .leaf_identifier()
        );
    }

    #[test]
    fn convert_block_node_into_mktree_nodes() {
        let block = block_node!(hash: "block_hash-5", num: 5, slot: 6);
        let expected: MKTreeNode = MKTreeNode::new(block.leaf_identifier());

        let computed_node: MKTreeNode = block.into();
        assert_eq!(expected, computed_node);
        assert_ne!(
            computed_node,
            block_node!(hash: "other", num: 5, slot: 6).into()
        );
        assert_ne!(
            computed_node,
            block_node!(hash: "block_hash-10", num: 1000, slot: 6).into()
        );
        assert_ne!(
            computed_node,
            block_node!(hash: "block_hash-10", num: 5, slot: 1000).into()
        );
    }

    #[test]
    fn convert_tx_node_into_mktree_nodes() {
        let transaction =
            tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 6);
        let expected: MKTreeNode = MKTreeNode::new(transaction.leaf_identifier());

        let computed_node: MKTreeNode = transaction.into();
        assert_eq!(expected, computed_node);
        assert_ne!(
            computed_node,
            tx_node!(hash: "other", block_hash: "block_hash-5", block: 5, slot: 6).into(),
        );
        assert_ne!(
            computed_node,
            tx_node!(hash: "tx_hash-5", block_hash: "other", block: 5, slot: 6).into(),
        );
        assert_ne!(
            computed_node,
            tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 1000, slot: 6).into(),
        );
        assert_ne!(
            computed_node,
            tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 1000).into(),
        );
    }

    mod mk_tree_node_ordering {
        use super::*;

        #[test]
        fn same_value_yield_equal_order() {
            let block = block_node!(num: 5, slot: 6);
            let tx = tx_node!(hash: "tx_hash-1", block: 5, slot: 6);

            assert_eq!(Ordering::Equal, block.cmp(&block));
            assert_eq!(Ordering::Equal, tx.cmp(&tx));
        }

        #[test]
        fn order_block_nodes_first_then_transaction_nodes() {
            let block = block_node!(num: 5, slot: 6);
            let tx = tx_node!(hash: "tx_hash-1", block: 5, slot: 6);

            assert_eq!(Ordering::Less, block.cmp(&tx));
            assert_eq!(Ordering::Greater, tx.cmp(&block));
        }

        #[test]
        fn order_blocks_by_block_number_first() {
            let block = block_node!(hash: "block_hash-5", num: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                block.cmp(&block_node!(hash: "block_hash-1", num: 10, slot: 1))
            );
            assert_eq!(
                Ordering::Greater,
                block.cmp(&block_node!(hash: "block_hash-9", num: 1, slot: 9))
            );
        }

        #[test]
        fn order_blocks_by_slot_number_second() {
            let block = block_node!(hash: "block_hash-5", num: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                block.cmp(&block_node!(hash: "block_hash-1", num: 5, slot: 9))
            );
            assert_eq!(
                Ordering::Greater,
                block.cmp(&block_node!(hash: "block_hash-9", num: 5, slot: 1))
            );
        }

        #[test]
        fn order_blocks_by_block_hash_third() {
            let block = block_node!(hash: "block_hash-5", num: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                block.cmp(&block_node!(hash: "block_hash-9", num: 5, slot: 6))
            );
            assert_eq!(
                Ordering::Greater,
                block.cmp(&block_node!(hash: "block_hash-1", num: 5, slot: 6))
            );
        }

        #[test]
        fn order_transactions_by_block_number_first() {
            let tx = tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                tx.cmp(
                    &tx_node!(hash: "tx_hash-1", block_hash: "block_hash-1", block: 10, slot: 1)
                )
            );
            assert_eq!(
                Ordering::Greater,
                tx.cmp(&tx_node!(hash: "tx_hash-9", block_hash: "block_hash-9", block: 1, slot: 9))
            );
        }

        #[test]
        fn order_transactions_by_slot_number_second() {
            let tx = tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                tx.cmp(&tx_node!(hash: "tx_hash-1", block_hash: "block_hash-1", block: 5, slot: 9))
            );
            assert_eq!(
                Ordering::Greater,
                tx.cmp(&tx_node!(hash: "tx_hash-9", block_hash: "block_hash-9", block: 5, slot: 1))
            );
        }

        #[test]
        fn order_transactions_by_block_hash_third() {
            let tx = tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                tx.cmp(&tx_node!(hash: "tx_hash-1", block_hash: "block_hash-9", block: 5, slot: 6))
            );
            assert_eq!(
                Ordering::Greater,
                tx.cmp(&tx_node!(hash: "tx_hash-9", block_hash: "block_hash-1", block: 5, slot: 6))
            );
        }

        #[test]
        fn order_transactions_by_transaction_hash_fourth() {
            let tx = tx_node!(hash: "tx_hash-5", block_hash: "block_hash-5", block: 5, slot: 6);

            assert_eq!(
                Ordering::Less,
                tx.cmp(&tx_node!(hash: "tx_hash-9", block_hash: "block_hash-5", block: 5, slot: 6))
            );
            assert_eq!(
                Ordering::Greater,
                tx.cmp(&tx_node!(hash: "tx_hash-1", block_hash: "block_hash-5", block: 5, slot: 6))
            );
        }

        #[test]
        fn sorting_a_vec() {
            let mut list = vec![
                tx_node!(hash: "tx_hash-70", block: 300, slot: 35),
                tx_node!(hash: "tx_hash-200", block: 100, slot: 100),
                tx_node!(hash: "tx_hash-50", block: 200, slot: 25),
                block_node!(num: 100, slot: 100),
                tx_node!(hash: "tx_hash-100", block: 100, slot: 100),
                block_node!(num: 200, slot: 35),
                block_node!(num: 200, slot: 25),
            ];
            list.sort();

            assert_eq!(
                list,
                vec![
                    block_node!(num: 100, slot: 100),
                    block_node!(num: 200, slot: 25),
                    block_node!(num: 200, slot: 35),
                    tx_node!(hash: "tx_hash-100", block: 100, slot: 100),
                    tx_node!(hash: "tx_hash-200", block: 100, slot: 100),
                    tx_node!(hash: "tx_hash-50", block: 200, slot: 25),
                    tx_node!(hash: "tx_hash-70", block: 300, slot: 35),
                ]
            );
        }
    }
}
