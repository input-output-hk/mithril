//! Merkelized map and associated proof

use anyhow::{anyhow, Context};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    rc::Rc,
};

use crate::{StdError, StdResult};

use super::{MKProof, MKTree, MKTreeNode};

/// The trait implemented by the keys of a MKMap
pub trait MKMapKey: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode> {}

/// A MKMap node
#[derive(Clone)]
pub enum MKMapNode<K: MKMapKey> {
    /// A MKMap node
    HashMap(Rc<MKMap<K>>),

    /// A MKMapProof node
    HashMapProof(MKMapProof<K>),

    /// A MKTree node
    Tree(Rc<MKTree>),

    /// A MKProof node
    Proof(MKProof),

    /// A MKTreeNode node
    TreeNode(MKTreeNode),
}

impl<K: MKMapKey> MKMapNode<K> {
    /// Get the root of the merkelized map node
    pub fn compute_root(&self) -> StdResult<MKTreeNode> {
        match self {
            MKMapNode::HashMap(mk_map) => mk_map.compute_root(),
            MKMapNode::HashMapProof(mk_map_proof) => Ok(mk_map_proof.compute_root().to_owned()),
            MKMapNode::Tree(merkle_tree) => merkle_tree.compute_root(),
            MKMapNode::Proof(merkle_tree_proof) => Ok(merkle_tree_proof.root().to_owned()),
            MKMapNode::TreeNode(merkle_tree_node) => Ok(merkle_tree_node.to_owned()),
        }
    }

    /// Check if the merkelized map node contains a leaf
    pub fn contains<T: Into<MKTreeNode> + Clone>(&self, leaf: &T) -> bool {
        let leaf = leaf.to_owned().into();
        match self {
            MKMapNode::HashMap(mk_map) => mk_map.contains(&leaf).is_some(),
            MKMapNode::HashMapProof(mk_map_proof) => mk_map_proof.contains(&leaf).is_ok(),
            MKMapNode::Tree(merkle_tree) => merkle_tree.contains(&leaf),
            MKMapNode::Proof(merkle_tree_proof) => {
                merkle_tree_proof.contains(&[leaf.to_owned()]).is_ok()
            }
            MKMapNode::TreeNode(merkle_tree_node) => *merkle_tree_node == leaf,
        }
    }
}

impl<K: MKMapKey> From<MKMap<K>> for MKMapNode<K> {
    fn from(other: MKMap<K>) -> Self {
        MKMapNode::HashMap(Rc::new(other))
    }
}

impl<K: MKMapKey> From<MKMapProof<K>> for MKMapNode<K> {
    fn from(other: MKMapProof<K>) -> Self {
        MKMapNode::HashMapProof(other)
    }
}

impl<K: MKMapKey> From<MKTree> for MKMapNode<K> {
    fn from(other: MKTree) -> Self {
        MKMapNode::Tree(Rc::new(other))
    }
}

impl<K: MKMapKey> From<MKProof> for MKMapNode<K> {
    fn from(other: MKProof) -> Self {
        MKMapNode::Proof(other)
    }
}

impl<K: MKMapKey> From<MKTreeNode> for MKMapNode<K> {
    fn from(other: MKTreeNode) -> Self {
        MKMapNode::TreeNode(other)
    }
}

impl<K: MKMapKey> TryFrom<MKMapNode<K>> for MKTreeNode {
    type Error = StdError;
    fn try_from(other: MKMapNode<K>) -> Result<Self, Self::Error> {
        other.compute_root()
    }
}

/// A MKMapProof that proves membership of an entry in the merkelized map
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct MKMapProof<K: MKMapKey> {
    master_proof: MKProof,
    sub_proofs: Vec<(K, MKMapProof<K>)>,
}

impl<K: MKMapKey> MKMapProof<K> {
    /// MKMapProof factory
    pub fn new(master_proof: MKProof, sub_proofs: BTreeMap<K, MKMapProof<K>>) -> Self {
        let sub_proofs = sub_proofs.into_iter().collect();
        Self {
            master_proof,
            sub_proofs,
        }
    }

    /// Get the root of the merkelized map proof
    pub fn compute_root(&self) -> MKTreeNode {
        self.master_proof.root().to_owned()
    }

    /// Verify the merkelized map proof
    pub fn verify(&self) -> StdResult<()> {
        for (_key, proof) in &self.sub_proofs {
            proof
                .verify()
                .with_context(|| "MKMapProof could not verify sub proof")?;
        }

        self.master_proof
            .verify()
            .with_context(|| "MKMapProof could not verify master proof")?;
        if !self.sub_proofs.is_empty() {
            self.master_proof
                .contains(
                    &self
                        .sub_proofs
                        .iter()
                        .map(|(k, p)| k.to_owned().into() + p.compute_root().to_owned())
                        .collect::<Vec<_>>(),
                )
                .with_context(|| "MKMapProof could not match verified leaves of master proof")?;
        }

        Ok(())
    }

    /// Check if the merkelized map proof contains a leaf
    pub fn contains(&self, leaf: &MKTreeNode) -> StdResult<()> {
        let master_proof_contains_leaf = self.master_proof.contains(&[leaf.to_owned()]).is_ok();
        let sub_proofs_contain_leaf = self
            .sub_proofs
            .iter()
            .any(|(_k, p)| p.contains(leaf).is_ok());
        (master_proof_contains_leaf || sub_proofs_contain_leaf)
            .then_some(())
            .ok_or(anyhow!("MKMapProof does not contain leaf {:?}", leaf))
    }
}

impl<K: MKMapKey> From<MKProof> for MKMapProof<K> {
    fn from(other: MKProof) -> Self {
        MKMapProof::new(other, BTreeMap::default())
    }
}

/// A MKMap, where the keys and values are merkelized and provable
pub struct MKMap<K: MKMapKey> {
    inner_map_values: BTreeMap<K, MKMapNode<K>>,
    inner_merkle_tree: MKTree,
}

impl<K: MKMapKey> MKMap<K> {
    /// MKMap factory
    pub fn new(entries: &[(K, MKMapNode<K>)]) -> StdResult<Self> {
        let inner_map_values = BTreeMap::default();
        let inner_merkle_tree = MKTree::new::<MKTreeNode>(&[])?;
        let mut mk_map = Self {
            inner_map_values,
            inner_merkle_tree,
        };
        let sorted_entries = BTreeMap::from_iter(entries.to_vec());
        for (key, value) in sorted_entries {
            mk_map.insert(key.clone(), value.clone())?;
        }

        Ok(mk_map)
    }

    /// Insert a new key-value pair
    /// Important: keys must be inserted in order to guarantee
    /// that the same set of key/values results in the same computation for the root.
    pub fn insert(&mut self, key: K, value: MKMapNode<K>) -> StdResult<()> {
        if let Some(existing_value) = self.inner_map_values.get(&key) {
            if existing_value.compute_root()? != value.compute_root()? {
                return Err(anyhow!(
                    "MKMap values should be replaced by entry with same root"
                ));
            }
        } else {
            let key_max = self.inner_map_values.keys().max();
            if key_max > Some(&key) {
                return Err(anyhow!("MKMap keys must be inserted in order"));
            }
        }

        self.inner_map_values.insert(key.clone(), value.clone());
        let mktree_node_value: MKTreeNode = value.try_into()?;
        let mktree_node_key: MKTreeNode = key.into();
        self.inner_merkle_tree
            .append(&[mktree_node_key + mktree_node_value])?;

        Ok(())
    }

    /// Check if the merkelized map contains a leaf (and returns the corresponding entry if exists)
    // TODO: optimize search ? maybe inverted sort ? Maybe filter the leaves to search in the corresponding entries ?
    pub fn contains(&self, leaf: &MKTreeNode) -> Option<(&K, &MKMapNode<K>)> {
        self.inner_map_values.iter().find(|(_, v)| v.contains(leaf))
    }

    /// Get the value of the merkelized map
    pub fn get(&self, key: &K) -> Option<&MKMapNode<K>> {
        self.inner_map_values.get(key)
    }

    /// Get an iterator for the key and values of the merkelized map
    pub fn iter(&self) -> impl Iterator<Item = (&K, &MKMapNode<K>)> {
        self.inner_map_values.iter()
    }

    /// Get the keys of the merkelized map
    pub fn keys(&self) -> Vec<K> {
        self.inner_map_values.keys().cloned().collect()
    }

    /// Get the values of the merkelized map
    pub fn values(&self) -> Vec<MKMapNode<K>> {
        self.inner_map_values.values().cloned().collect()
    }

    /// Get the merkle tree of the merkelized map
    pub fn merkle_tree(&self) -> &MKTree {
        &self.inner_merkle_tree
    }

    /// Compress the merkelized map
    /// Note: the returned merkelized hash has all its values compressed to their root representation
    pub fn compress(&self) -> StdResult<Self> {
        Self::new(
            self.inner_map_values
                .iter()
                .try_fold(vec![], |mut acc, (k, v)| -> StdResult<Vec<_>> {
                    acc.push((k.to_owned(), MKMapNode::TreeNode(v.compute_root()?)));
                    Ok(acc)
                })?
                .as_slice(),
        )
    }

    /// Get the root of the merkle tree of the merkelized map
    pub fn compute_root(&self) -> StdResult<MKTreeNode> {
        self.inner_merkle_tree.compute_root()
    }

    /// Get the proof for a set of values of the merkelized map (recursively if needed)
    pub fn compute_proof<T: Into<MKTreeNode> + Clone>(
        &self,
        leaves: &[T],
    ) -> StdResult<MKMapProof<K>> {
        let leaves_by_keys: HashMap<K, Vec<MKTreeNode>> = leaves
            .iter()
            .filter_map(|leaf| match self.contains(&leaf.to_owned().into()) {
                Some((key, MKMapNode::Tree(_))) => Some((key.to_owned(), leaf)),
                Some((key, MKMapNode::HashMap(_))) => Some((key.to_owned(), leaf)),
                _ => None,
            })
            .fold(HashMap::default(), |mut acc, (key, leaf)| {
                acc.entry(key.to_owned())
                    .or_default()
                    .push(leaf.to_owned().into());
                acc
            });

        let mut sub_proofs = BTreeMap::<K, MKMapProof<K>>::default();
        for (key, sub_leaves) in leaves_by_keys {
            match self.inner_map_values.get(&key) {
                Some(MKMapNode::Tree(ref value)) => {
                    let proof = value
                        .compute_proof(&sub_leaves)
                        .with_context(|| "MKMap could not compute sub proof for MKTree")?;
                    sub_proofs.insert(key.to_owned(), proof.into());
                }
                Some(MKMapNode::HashMap(ref value)) => {
                    let proof = value
                        .compute_proof(&sub_leaves)
                        .with_context(|| "MKMap could not compute sub proof for MKMap")?;
                    sub_proofs.insert(key.to_owned(), proof);
                }
                _ => {}
            }
        }

        let master_proof = self
            .inner_merkle_tree
            .compute_proof(
                &sub_proofs
                    .iter()
                    .map(|(k, p)| k.to_owned().into() + p.compute_root().to_owned())
                    .collect::<Vec<MKTreeNode>>(),
            )
            .with_context(|| "MKMap could not compute master proof")?;

        Ok(MKMapProof::new(master_proof, sub_proofs))
    }
}

impl<K: MKMapKey> Clone for MKMap<K> {
    fn clone(&self) -> Self {
        // Cloning should never fail so uwnrap is safe
        let mut clone = Self::new(&[]).unwrap();
        for (k, v) in self.inner_map_values.iter() {
            clone.insert(k.to_owned(), v.to_owned()).unwrap();
        }

        clone
    }
}

impl<'a, K: MKMapKey> From<&'a MKMap<K>> for &'a MKTree {
    fn from(other: &'a MKMap<K>) -> Self {
        other.merkle_tree()
    }
}

impl<K: MKMapKey> TryFrom<MKMap<K>> for MKTreeNode {
    type Error = StdError;
    fn try_from(other: MKMap<K>) -> Result<Self, Self::Error> {
        other.compute_root()
    }
}

#[cfg(test)]
mod tests {

    use std::collections::BTreeSet;
    use std::ops::Range;

    use crate::entities::BlockRange;

    use super::*;

    fn generate_merkle_trees(
        total_leaves: u64,
        block_range_length: u64,
    ) -> Vec<(BlockRange, MKTree)> {
        (0..total_leaves / block_range_length)
            .map(|block_range_index| {
                let block_range = BlockRange::new(
                    block_range_index * block_range_length,
                    (block_range_index + 1) * block_range_length,
                );
                let leaves = <Range<u64> as Clone>::clone(&block_range)
                    .map(|leaf_index| leaf_index.to_string())
                    .collect::<Vec<_>>();
                let merkle_tree_block_range = MKTree::new(&leaves).unwrap();

                (block_range, merkle_tree_block_range)
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn test_mk_map_should_compute_consistent_root() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_node_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let merkle_tree_proof_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKMapNode::Proof::<BlockRange>(
                        mktree.compute_proof(&mktree.leaves()[..1]).unwrap(),
                    ),
                )
            })
            .collect::<Vec<_>>();
        let merkle_tree_full_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();

        let mk_map_nodes = MKMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_map_proofs = MKMap::new(merkle_tree_proof_entries.as_slice()).unwrap();
        let mk_map_full = MKMap::new(merkle_tree_full_entries.as_slice()).unwrap();

        let mk_map_nodes_root = mk_map_nodes.compute_root().unwrap();
        let mk_map_proofs_root = mk_map_proofs.compute_root().unwrap();
        let mk_map_full_root = mk_map_full.compute_root().unwrap();

        assert_eq!(mk_map_full_root, mk_map_nodes_root);
        assert_eq!(mk_map_full_root, mk_map_proofs_root);
    }

    #[test]
    fn test_mk_map_should_accept_replacement_with_same_root_value() {
        let entries = generate_merkle_trees(1000, 10);
        let merkle_tree_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mut mk_map = MKMap::new(merkle_tree_entries.as_slice()).unwrap();
        let block_range_replacement = BlockRange::new(0, 10);
        let same_root_value = MKMapNode::TreeNode(
            mk_map
                .get(&block_range_replacement)
                .unwrap()
                .compute_root()
                .unwrap(),
        );
        mk_map
            .insert(block_range_replacement, same_root_value)
            .unwrap();
    }

    #[test]
    fn test_mk_map_should_reject_replacement_with_different_root_value() {
        let entries = generate_merkle_trees(1000, 10);
        let merkle_tree_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mut mk_map = MKMap::new(merkle_tree_entries.as_slice()).unwrap();
        let block_range_replacement = BlockRange::new(0, 10);
        let value_replacement: MKTreeNode = "test-123".to_string().into();
        let different_root_value = MKMapNode::TreeNode(value_replacement);
        mk_map
            .insert(block_range_replacement, different_root_value)
            .expect_err("the MKMap should reject replacement with different root value");
    }

    #[test]
    fn test_mk_map_should_reject_out_of_order_insertion() {
        let entries = generate_merkle_trees(1000, 10);
        let merkle_tree_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let mut mk_map = MKMap::new(merkle_tree_entries.as_slice()).unwrap();
        let out_of_order_entry = (
            BlockRange::new(0, 25),
            MKMapNode::TreeNode("test-123".into()),
        );
        mk_map
            .insert(out_of_order_entry.0, out_of_order_entry.1)
            .expect_err("the MKMap should reject out of order insertion");
    }

    #[test]
    fn test_mk_map_should_list_keys_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let mk_map = MKMap::new(merkle_tree_entries.as_slice()).unwrap();
        let keys = mk_map.keys();
        let expected_keys = merkle_tree_entries
            .iter()
            .map(|(k, _)| k)
            .cloned()
            .collect::<Vec<_>>();

        assert_eq!(expected_keys, keys);
    }

    #[test]
    fn test_mk_map_should_list_values_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let mk_map = MKMap::new(merkle_tree_entries.as_slice()).unwrap();
        let values = mk_map.values();
        let expected_values = merkle_tree_entries
            .iter()
            .map(|(_, v)| v)
            .cloned()
            .collect::<Vec<_>>();

        assert_eq!(
            BTreeSet::from_iter(expected_values.iter().map(|v| v.compute_root().unwrap())),
            BTreeSet::from_iter(values.iter().map(|v| v.compute_root().unwrap()))
        );
    }

    #[test]
    fn test_mk_map_should_find_value_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let mktree_node_to_certify = entries[2].1.leaves()[10].clone();
        let merkle_tree_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mk_map_full = MKMap::new(merkle_tree_entries.as_slice()).unwrap();

        mk_map_full.contains(&mktree_node_to_certify).unwrap();
    }

    #[test]
    fn test_mk_map_should_compress_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_node_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mk_map_full = MKMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_map_compressed = mk_map_full.compress().unwrap();
        let mk_map_root = mk_map_full.compute_root().unwrap();
        let mk_map_proof_compressed = mk_map_compressed.compute_root().unwrap();

        assert_eq!(mk_map_root, mk_map_proof_compressed);
    }

    #[test]
    fn test_mk_map_should_clone_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_node_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mk_map = MKMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_map_clone = mk_map.clone();

        assert_eq!(
            mk_map.compute_root().unwrap(),
            mk_map_clone.compute_root().unwrap(),
        );
    }

    #[test]
    fn test_mk_map_should_compute_and_verify_valid_proof() {
        let entries = generate_merkle_trees(100000, 100);
        let mktree_nodes_to_certify = [
            entries[0].1.leaves()[0].clone(),
            entries[2].1.leaves()[0].clone(),
            entries[2].1.leaves()[5].clone(),
            entries[3].1.leaves()[10].clone(),
        ];
        let merkle_tree_node_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mk_map_full = MKMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_map_proof = mk_map_full.compute_proof(&mktree_nodes_to_certify).unwrap();

        mk_map_proof.verify().unwrap();

        let map_proof_root = mk_map_proof.compute_root();
        let map_proof_root_expected = mk_map_full.compute_root().unwrap();
        assert_eq!(map_proof_root, map_proof_root_expected);
    }

    #[test]
    fn test_mk_map_should_compute_and_verify_valid_proof_recursively() {
        let entries = generate_merkle_trees(100000, 100);
        let mktree_nodes_to_certify = [
            entries[0].1.leaves()[0].clone(),
            entries[2].1.leaves()[5].clone(),
            entries[3].1.leaves()[10].clone(),
            entries[20].1.leaves()[0].clone(),
            entries[30].1.leaves()[0].clone(),
        ];
        let merkle_tree_node_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), MKMapNode::Tree(Rc::new(mktree))))
            .collect::<Vec<_>>()
            .chunks(10)
            .map(|entries| {
                (
                    entries
                        .iter()
                        .fold(BlockRange::new(0, 0), |mut acc, (range, _)| {
                            acc.try_add(range).unwrap()
                        }),
                    MKMapNode::HashMap(Rc::new(MKMap::new(entries).unwrap())),
                )
            })
            .collect::<Vec<_>>();

        let mk_map_full = MKMap::new(merkle_tree_node_entries.as_slice()).unwrap();

        let mk_map_proof = mk_map_full.compute_proof(&mktree_nodes_to_certify).unwrap();

        mk_map_proof.verify().unwrap();

        let map_proof_root = mk_map_proof.compute_root();
        let map_proof_root_expected = mk_map_full.compute_root().unwrap();
        assert_eq!(map_proof_root, map_proof_root_expected);
    }
}
