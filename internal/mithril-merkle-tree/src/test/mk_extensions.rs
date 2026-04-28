use anyhow::Context;

use crate::{
    MKMap, MKMapKey, MKMapValue, MKProof, MKTree, MKTreeNode, MKTreeStoreInMemory, MKTreeStorer,
    StdResult,
};

/// Extension trait adding test utilities to [MKTree]
pub trait MKTreeTestExtension {
    /// `TEST ONLY` - Generate the root of the Merkle tree built from the given leaves.
    ///
    /// Shortcut for `MKTree::new_from_iter(leaves)?.compute_root()`.
    fn compute_root_from_iter<T: IntoIterator<Item = U>, U: Into<MKTreeNode>>(
        leaves: T,
    ) -> StdResult<MKTreeNode>;
}

impl<S: MKTreeStorer> MKTreeTestExtension for MKTree<S> {
    fn compute_root_from_iter<T: IntoIterator<Item = U>, U: Into<MKTreeNode>>(
        leaves: T,
    ) -> StdResult<MKTreeNode> {
        let mk_tree = Self::new_from_iter(leaves)?;
        mk_tree.compute_root()
    }
}

/// Extension trait adding test utilities to [MKProof]
pub trait MKProofTestExtension {
    /// `TEST ONLY` - Build a [MKProof] based on the given leaves.
    fn from_leaves<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<MKProof>;

    /// `TEST ONLY` - Build a [MKProof] from a subset of leaves to verify against the full set.
    fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof>;
}

impl MKProofTestExtension for MKProof {
    fn from_leaves<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<MKProof> {
        Self::from_subset_of_leaves(leaves, leaves)
    }

    fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
        leaves: &[T],
        leaves_to_verify: &[T],
    ) -> StdResult<MKProof> {
        fn list_to_mknode<T: Into<MKTreeNode> + Clone>(hashes: &[T]) -> Vec<MKTreeNode> {
            hashes.iter().map(|h| h.clone().into()).collect()
        }

        let leaves = list_to_mknode(leaves);
        let leaves_to_verify = list_to_mknode(leaves_to_verify);

        let mktree = MKTree::<MKTreeStoreInMemory>::new(&leaves)
            .with_context(|| "MKTree creation should not fail")?;
        mktree.compute_proof(&leaves_to_verify)
    }
}

/// Extension trait adding test utilities to [MKMap]
pub trait MKMapTestExtension<K, V, S: MKTreeStorer> {
    /// `TEST ONLY` - Get the root of the merkle tree of a merkelized map built from an iterator
    ///
    /// Shortcut for `MKMap::new_from_iter(entries)?.compute_root()`
    fn compute_root_from_iter<T: IntoIterator<Item = (K, V)>>(entries: T) -> StdResult<MKTreeNode>;
}

impl<K: MKMapKey, V: MKMapValue<K>, S: MKTreeStorer> MKMapTestExtension<K, V, S>
    for MKMap<K, V, S>
{
    fn compute_root_from_iter<T: IntoIterator<Item = (K, V)>>(entries: T) -> StdResult<MKTreeNode> {
        let mk_map = Self::new_from_iter(entries)?;
        mk_map.compute_root()
    }
}
