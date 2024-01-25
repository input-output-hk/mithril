use crate::crypto_helper::MKTreeNode;

use super::ImmutableFileNumber;

/// TransactionHash is the unique identifier of a cardano transaction.
pub type TransactionHash = String;

/// BlockNumber is the block number of a cardano transaction.
pub type BlockNumber = u64;

#[derive(Debug, PartialEq, Clone)]
/// Cardano transaction representation
pub struct CardanoTransaction {
    /// Unique hash of the transaction
    pub transaction_hash: TransactionHash,

    /// Block number of the transaction
    pub block_number: BlockNumber,

    /// Immutable file number of the transaction
    pub immutable_file_number: ImmutableFileNumber,
}

impl CardanoTransaction {
    /// CardanoTransaction factory
    pub fn new(
        hash: &str,
        block_number: BlockNumber,
        immutable_file_number: ImmutableFileNumber,
    ) -> Self {
        Self {
            transaction_hash: hash.to_owned(),
            block_number,
            immutable_file_number,
        }
    }
}

impl From<CardanoTransaction> for MKTreeNode {
    fn from(other: CardanoTransaction) -> Self {
        (&other).into()
    }
}

impl From<&CardanoTransaction> for MKTreeNode {
    fn from(other: &CardanoTransaction) -> Self {
        MKTreeNode::new(other.transaction_hash.as_bytes().to_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_cardano_transaction_to_merkle_tree_node() {
        let transaction = CardanoTransaction {
            transaction_hash: "tx-hash-123".to_string(),
            block_number: 1,
            immutable_file_number: 1,
        };
        let computed_mktree_node: MKTreeNode = transaction.into();
        let expected_mk_tree_node = MKTreeNode::new("tx-hash-123".as_bytes().to_vec());
        let non_expected_mk_tree_node = MKTreeNode::new("tx-hash-456".as_bytes().to_vec());

        assert_eq!(expected_mk_tree_node, computed_mktree_node);
        assert_ne!(non_expected_mk_tree_node, computed_mktree_node);
    }
}
