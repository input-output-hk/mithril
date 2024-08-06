use crate::{
    crypto_helper::MKTreeNode,
    entities::{BlockHash, BlockNumber, SlotNumber},
};

/// TransactionHash is the unique identifier of a cardano transaction.
pub type TransactionHash = String;

#[derive(Debug, PartialEq, Clone)]
/// Cardano transaction representation
pub struct CardanoTransaction {
    /// Unique hash of the transaction
    pub transaction_hash: TransactionHash,

    /// Block number of the transaction
    pub block_number: BlockNumber,

    /// Slot number of the transaction
    pub slot_number: SlotNumber,

    /// Block hash of the transaction
    pub block_hash: BlockHash,
}

impl CardanoTransaction {
    /// CardanoTransaction factory
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
        let transaction =
            CardanoTransaction::new("tx-hash-123", BlockNumber(10), SlotNumber(4), "block_hash");

        let computed_mktree_node: MKTreeNode = transaction.into();
        let expected_mk_tree_node = MKTreeNode::new("tx-hash-123".as_bytes().to_vec());
        let non_expected_mk_tree_node = MKTreeNode::new("tx-hash-456".as_bytes().to_vec());

        assert_eq!(expected_mk_tree_node, computed_mktree_node);
        assert_ne!(non_expected_mk_tree_node, computed_mktree_node);
    }
}
