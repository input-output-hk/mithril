use crate::StdResult;
use crate::crypto_helper::{MKMapProof, MKTreeNode, ProtocolMkProof};
use crate::entities::{
    BlockHash, BlockRange, CardanoBlock, CardanoBlockTransactionMkTreeNode, CardanoTransaction,
    TransactionHash,
};

/// Trait to convert a type into a [MKTreeNode]
///
/// Note: this trait exists only to allow `CardanoTransaction` to have a second, different, conversion
/// to [MkTreeNode] without changing its existing implementation.
/// The existing implementation cover proof generation and certification for `CardanoTransactions`
/// signed entity, this trait implementation cover the same operation for `CardanoBlocksTransactions`
/// signed entity.
///
/// Todo: after `CardanoTransactions` signed entity removal, remove this trait and use `Into<MKTreeNode>` instead.
pub trait IntoMKTreeNode {
    /// Converts the item into a [MKTreeNode]
    fn into_mk_tree_node(self) -> MKTreeNode;
}

/// A cryptographic proof of that a set of items is included in a Merkle tree
#[derive(Clone, Debug, PartialEq)]
pub struct MkSetProof<T: IntoMKTreeNode + Clone> {
    /// Certified blocks with their transactions hashes included
    pub(crate) items: Vec<T>,

    /// Proof of inclusion of the Blocks and transactions
    pub(crate) proof: ProtocolMkProof,
}

impl<T: IntoMKTreeNode + Clone> MkSetProof<T> {
    /// MkSetProof factory
    pub fn new<P: Into<MKMapProof<BlockRange>>>(items: Vec<T>, proof: P) -> Self {
        Self {
            items,
            proof: ProtocolMkProof::new(proof.into()),
        }
    }

    /// Return the hex encoded merkle root of this proof
    pub fn merkle_root(&self) -> String {
        self.proof.compute_root().to_hex()
    }

    /// Verify that proof includes all items of the set
    pub fn verify(&self) -> StdResult<()> {
        self.proof.verify()?;
        for node in self.items.iter().cloned().map(IntoMKTreeNode::into_mk_tree_node) {
            self.proof.contains(&node)?;
        }

        Ok(())
    }
}

impl IntoMKTreeNode for CardanoBlock {
    fn into_mk_tree_node(self) -> MKTreeNode {
        let node: CardanoBlockTransactionMkTreeNode = self.into();
        node.into()
    }
}

impl MkSetProof<CardanoBlock> {
    /// Get the hashes of the blocks certified by this proof
    pub fn blocks_hashes(&self) -> impl Iterator<Item = &BlockHash> + '_ {
        self.items.iter().map(|b| &b.block_hash)
    }
}

impl IntoMKTreeNode for CardanoTransaction {
    fn into_mk_tree_node(self) -> MKTreeNode {
        let node: CardanoBlockTransactionMkTreeNode = self.into();
        node.into()
    }
}

impl MkSetProof<CardanoTransaction> {
    /// Get the hashes of the transactions certified by this proof
    pub fn transactions_hashes(&self) -> impl Iterator<Item = &TransactionHash> + '_ {
        self.items.iter().map(|t| &t.transaction_hash)
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeStoreInMemory};
    use crate::test::entities_extensions::BlockRangeTestExtension;

    use super::*;

    impl IntoMKTreeNode for &str {
        fn into_mk_tree_node(self) -> MKTreeNode {
            MKTreeNode::new(self.as_bytes().to_vec())
        }
    }

    fn tamper<T: Clone>(item: &T, f: fn(&mut T)) -> T {
        let mut item = item.clone();
        f(&mut item);
        item
    }

    fn mk_proof_for(items: &[&str]) -> MKMapProof<BlockRange> {
        let mk_map: MKMap<_, MKMapNode<BlockRange, MKTreeStoreInMemory>, MKTreeStoreInMemory> =
            MKMap::new(&[(BlockRange::new(0, 100), MKTree::new(items).unwrap().into())]).unwrap();
        mk_map.compute_proof(items).unwrap()
    }

    #[test]
    fn should_verify_where_all_items_are_contained_in_the_proof() {
        let leaves = vec!["leaf-1", "leaf-2", "leaf-3", "leaf-4", "leaf-5", "leaf-6"];
        let mk_map_proof = mk_proof_for(&leaves);

        let proof = MkSetProof::new(leaves, mk_map_proof);

        proof.verify().expect("The proof should be valid");
    }

    #[test]
    fn should_not_verify_where_at_least_one_item_is_not_contained_in_the_proof() {
        let proved_leaves = vec!["leaf-1", "leaf-2", "leaf-3", "leaf-4", "leaf-5", "leaf-6"];
        let mk_map_proof = mk_proof_for(&proved_leaves);
        let tampered_leaves = tamper(&proved_leaves, |l| l.push("tampered-leaf"));

        let proof = MkSetProof::new(tampered_leaves, mk_map_proof);

        proof.verify().expect_err("The proof should be invalid");
    }
}
