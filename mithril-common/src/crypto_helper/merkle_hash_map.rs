//! Merkelized hash map and proof

use anyhow::{anyhow, Context};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    rc::Rc,
};

use crate::{StdError, StdResult};

use super::{MKProof, MKTree, MKTreeNode};

/// A MKHashMap node
#[derive(Clone)]
pub enum MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    /// A MKHashMap node
    HashMap(Rc<MKHashMap<K>>),

    /// A MKHashMapProof node
    HashMapProof(MKHashMapProof<K>),

    /// A MKTree node
    Tree(Rc<MKTree>),

    /// A MKProof node
    Proof(MKProof),

    /// A MKTreeNode node
    TreeNode(MKTreeNode),
}

impl<K> MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    /// Get the root of the merkelized hash map node
    pub fn compute_root(&self) -> StdResult<MKTreeNode> {
        match self {
            MKHashMapNode::HashMap(mk_hash_map) => mk_hash_map.compute_root(),
            MKHashMapNode::HashMapProof(mk_hash_proof) => {
                Ok(mk_hash_proof.compute_root().to_owned())
            }
            MKHashMapNode::Tree(merkle_tree) => merkle_tree.compute_root(),
            MKHashMapNode::Proof(merkle_tree_proof) => Ok(merkle_tree_proof.root().to_owned()),
            MKHashMapNode::TreeNode(merkle_tree_node) => Ok(merkle_tree_node.to_owned()),
        }
    }

    /// Check if the merkelized hash map node contains a leaf
    pub fn contains<T: Into<MKTreeNode> + Clone>(&self, leaf: &T) -> bool {
        let leaf = leaf.to_owned().into();
        match self {
            MKHashMapNode::HashMap(mk_hash_map) => mk_hash_map.contains(&leaf).is_some(),
            MKHashMapNode::HashMapProof(mk_hash_proof) => mk_hash_proof.contains(&leaf).is_ok(),
            MKHashMapNode::Tree(merkle_tree) => merkle_tree.contains(&leaf),
            MKHashMapNode::Proof(merkle_tree_proof) => {
                merkle_tree_proof.contains(&[leaf.to_owned()]).is_ok()
            }
            MKHashMapNode::TreeNode(merkle_tree_node) => *merkle_tree_node == leaf,
        }
    }
}

impl<K> From<MKHashMap<K>> for MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: MKHashMap<K>) -> Self {
        MKHashMapNode::HashMap(Rc::new(other))
    }
}

impl<K> From<MKHashMapProof<K>> for MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: MKHashMapProof<K>) -> Self {
        MKHashMapNode::HashMapProof(other)
    }
}

impl<K> From<MKTree> for MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: MKTree) -> Self {
        MKHashMapNode::Tree(Rc::new(other))
    }
}

impl<K> From<MKProof> for MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: MKProof) -> Self {
        MKHashMapNode::Proof(other)
    }
}

impl<K> From<MKTreeNode> for MKHashMapNode<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: MKTreeNode) -> Self {
        MKHashMapNode::TreeNode(other)
    }
}

impl<K> TryFrom<MKHashMapNode<K>> for MKTreeNode
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    type Error = StdError;
    fn try_from(other: MKHashMapNode<K>) -> Result<Self, Self::Error> {
        other.compute_root()
    }
}

/// A MKHashMapProof that proves membership of an entry in the merkelized hash map
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct MKHashMapProof<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    master_proof: MKProof,
    sub_proofs: BTreeMap<K, MKHashMapProof<K>>,
}

impl<K> MKHashMapProof<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    /// MKHashMapProof factory
    pub fn new(master_proof: MKProof, sub_proofs: HashMap<K, MKHashMapProof<K>>) -> Self {
        let sub_proofs = sub_proofs.into_iter().collect();
        Self {
            master_proof,
            sub_proofs,
        }
    }

    /// Get the root of the merkelized hash map proof
    pub fn compute_root(&self) -> MKTreeNode {
        self.master_proof.root().to_owned()
    }

    /// Verify the merkelized hash map proof
    // TODO: parallelize proof verification? Is it compatible with WASM?
    pub fn verify(&self) -> StdResult<()> {
        for proof in self.sub_proofs.values() {
            proof
                .verify()
                .with_context(|| "MKHashMapProof could not verify sub proof")?;
        }

        self.master_proof
            .verify()
            .with_context(|| "MKHashMapProof could not verify master proof")?;
        if !self.sub_proofs.is_empty() {
            self.master_proof
                .contains(
                    &self
                        .sub_proofs
                        .iter()
                        .map(|(k, p)| k.to_owned().into() + p.compute_root().to_owned())
                        .collect::<Vec<_>>(),
                )
                .with_context(|| {
                    "MKHashMapProof could not match verified leaves of master proof"
                })?;
        }

        Ok(())
    }

    /// Check if the merkelized hash map proof contains a leaf
    pub fn contains(&self, leaf: &MKTreeNode) -> StdResult<()> {
        let master_proof_contains_leaf = self.master_proof.contains(&[leaf.to_owned()]).is_ok();
        let sub_proofs_contain_leaf = self
            .sub_proofs
            .iter()
            .any(|(_k, p)| p.contains(leaf).is_ok());
        (master_proof_contains_leaf || sub_proofs_contain_leaf)
            .then_some(())
            .ok_or(anyhow!("MKHashMapProof does not contain leaf {:?}", leaf))
    }
}

impl<K> From<MKProof> for MKHashMapProof<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: MKProof) -> Self {
        MKHashMapProof::new(other, HashMap::default())
    }
}

/// A MKHashMap, where the keys and values are merkelized and provable
pub struct MKHashMap<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    inner_map_values: BTreeMap<K, MKHashMapNode<K>>,
    inner_merkle_tree: MKTree,
}

impl<K> MKHashMap<K>
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    /// MKHashMap factory
    pub fn new(entries: &[(K, MKHashMapNode<K>)]) -> StdResult<Self> {
        let inner_map_values = BTreeMap::default();
        let inner_merkle_tree = MKTree::new::<MKTreeNode>(&[])?;
        let mut mk_hash_map = Self {
            inner_map_values,
            inner_merkle_tree,
        };
        let sorted_entries = BTreeMap::from_iter(entries.to_vec());
        for (key, value) in sorted_entries {
            mk_hash_map.insert(key.clone(), value.clone())?;
        }
        Ok(mk_hash_map)
    }

    /// Insert a new key-value pair
    /// Important: keys must be inserted in order to guarantee
    /// that the same set of key/values results in the same computation for the root.
    pub fn insert(&mut self, key: K, value: MKHashMapNode<K>) -> StdResult<()> {
        let key_max = self.inner_map_values.keys().max();
        if key_max > Some(&key) {
            return Err(anyhow!("MKHashMap keys must be inserted in order"));
        }
        self.inner_map_values.insert(key.clone(), value.clone());
        let mktree_node_value: MKTreeNode = value.try_into()?;
        let mktree_node_key: MKTreeNode = key.into();
        self.inner_merkle_tree
            .append(&[mktree_node_key + mktree_node_value])?;

        Ok(())
    }

    /// Check if the merkelized hash map contains a leaf (and returns the corresponding entry if exists)
    // TODO: optimize search ? maybe inverted sort ? Maybe filter the leaves to search in the corresponding entries ?
    pub fn contains(&self, leaf: &MKTreeNode) -> Option<(&K, &MKHashMapNode<K>)> {
        self.inner_map_values.iter().find(|(_, v)| v.contains(leaf))
    }

    /// Get an iterator for the key and values of the merkelized hash map
    pub fn iter(&self) -> impl Iterator<Item = (&K, &MKHashMapNode<K>)> {
        self.inner_map_values.iter()
    }

    /// Get the keys of the merkelized hash map
    pub fn keys(&self) -> Vec<K> {
        self.inner_map_values.keys().cloned().collect()
    }

    /// Get the values of the merkelized hash map
    pub fn values(&self) -> Vec<MKHashMapNode<K>> {
        self.inner_map_values.values().cloned().collect()
    }

    /// Get the merkle tree of the merkelized hash map
    pub fn merkle_tree(&self) -> &MKTree {
        &self.inner_merkle_tree
    }

    /// Compress the merkelized hash map
    /// Note: the returned merkelized hash has all its values compressed to their root representation
    pub fn compress(&self) -> StdResult<Self> {
        Self::new(
            self.inner_map_values
                .iter()
                .try_fold(vec![], |mut acc, (k, v)| -> StdResult<Vec<_>> {
                    acc.push((k.to_owned(), MKHashMapNode::TreeNode(v.compute_root()?)));
                    Ok(acc)
                })?
                .as_slice(),
        )
    }

    /// Get the root of the merkle tree of the merkelized hash map
    pub fn compute_root(&self) -> StdResult<MKTreeNode> {
        self.inner_merkle_tree.compute_root()
    }

    /// Get the proof for a set on values
    // TODO: parallelize proof generation
    pub fn compute_proof<T: Into<MKTreeNode> + Clone>(
        &self,
        leaves: &[T],
    ) -> StdResult<MKHashMapProof<K>> {
        let leaves_by_keys: HashMap<K, Vec<MKTreeNode>> = leaves
            .iter()
            .filter_map(|leaf| match self.contains(&leaf.to_owned().into()) {
                Some((key, MKHashMapNode::Tree(_))) => Some((key.to_owned(), leaf)),
                Some((key, MKHashMapNode::HashMap(_))) => Some((key.to_owned(), leaf)),
                _ => None,
            })
            .fold(HashMap::default(), |mut acc, (key, leaf)| {
                acc.entry(key.to_owned())
                    .or_default()
                    .push(leaf.to_owned().into());
                acc
            });

        let mut sub_proofs = HashMap::<K, MKHashMapProof<K>>::default();
        for (key, sub_leaves) in leaves_by_keys {
            match self.inner_map_values.get(&key) {
                Some(MKHashMapNode::Tree(ref value)) => {
                    let proof = value
                        .compute_proof(&sub_leaves)
                        .with_context(|| "MKHashMap could not compute sub proof for MKTree")?;
                    sub_proofs.insert(key.to_owned(), proof.into());
                }
                Some(MKHashMapNode::HashMap(ref value)) => {
                    let proof = value
                        .compute_proof(&sub_leaves)
                        .with_context(|| "MKHashMap could not compute sub proof for MKHashMap")?;
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
            .with_context(|| "MKHashMap could not compute master proof")?;

        Ok(MKHashMapProof::new(master_proof, sub_proofs))
    }
}

impl<'a, K> From<&'a MKHashMap<K>> for &'a MKTree
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    fn from(other: &'a MKHashMap<K>) -> Self {
        other.merkle_tree()
    }
}

impl<K> TryFrom<MKHashMap<K>> for MKTreeNode
where
    K: PartialEq + Eq + PartialOrd + Ord + Clone + Hash + Into<MKTreeNode>,
{
    type Error = StdError;
    fn try_from(other: MKHashMap<K>) -> Result<Self, Self::Error> {
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
    fn test_mk_hash_map_should_compute_consistent_root() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_node_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKHashMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let merkle_tree_proof_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKHashMapNode::Proof::<BlockRange>(
                        mktree.compute_proof(&mktree.leaves()[..1]).unwrap(),
                    ),
                )
            })
            .collect::<Vec<_>>();
        let merkle_tree_full_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();

        let mk_hash_map_nodes = MKHashMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_hash_map_proofs = MKHashMap::new(merkle_tree_proof_entries.as_slice()).unwrap();
        let mk_hash_map_full = MKHashMap::new(merkle_tree_full_entries.as_slice()).unwrap();

        let mk_hash_map_nodes_root = mk_hash_map_nodes.compute_root().unwrap();
        let mk_hash_map_proofs_root = mk_hash_map_proofs.compute_root().unwrap();
        let mk_hash_map_full_root = mk_hash_map_full.compute_root().unwrap();

        assert_eq!(mk_hash_map_full_root, mk_hash_map_nodes_root);
        assert_eq!(mk_hash_map_full_root, mk_hash_map_proofs_root);
    }

    #[test]
    fn test_mk_hash_map_should_reject_out_of_order_insertion() {
        let entries = generate_merkle_trees(1000, 10);
        let merkle_tree_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKHashMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let mut mk_hash_map = MKHashMap::new(merkle_tree_entries.as_slice()).unwrap();
        let out_of_order_entry = (
            BlockRange::new(0, 25),
            MKHashMapNode::TreeNode("test-123".into()),
        );
        mk_hash_map
            .insert(out_of_order_entry.0, out_of_order_entry.1)
            .expect_err("the MKHashMap should reject out of order insertion");
    }

    #[test]
    fn test_mk_hash_map_should_list_keys_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKHashMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let mk_hash_map = MKHashMap::new(merkle_tree_entries.as_slice()).unwrap();
        let keys = mk_hash_map.keys();
        let expected_keys = merkle_tree_entries
            .iter()
            .map(|(k, _)| k)
            .cloned()
            .collect::<Vec<_>>();

        assert_eq!(expected_keys, keys);
    }

    #[test]
    fn test_mk_hash_map_should_list_values_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_entries = &entries
            .iter()
            .map(|(range, mktree)| {
                (
                    range.to_owned(),
                    MKHashMapNode::TreeNode(mktree.try_into().unwrap()),
                )
            })
            .collect::<Vec<_>>();
        let mk_hash_map = MKHashMap::new(merkle_tree_entries.as_slice()).unwrap();
        let values = mk_hash_map.values();
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
    fn test_mk_hash_map_should_find_value_correctly() {
        let entries = generate_merkle_trees(100000, 100);
        let mktree_node_to_certify = entries[2].1.leaves()[10].clone();
        let merkle_tree_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mk_hash_map_full = MKHashMap::new(merkle_tree_entries.as_slice()).unwrap();

        mk_hash_map_full.contains(&mktree_node_to_certify).unwrap();
    }

    #[test]
    fn test_mk_hash_map_should_compress_correctlty() {
        let entries = generate_merkle_trees(100000, 100);
        let merkle_tree_node_entries = &entries
            .into_iter()
            .map(|(range, mktree)| (range.to_owned(), mktree.into()))
            .collect::<Vec<_>>();
        let mk_hash_map_full = MKHashMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_hash_map_compressed = mk_hash_map_full.compress().unwrap();
        let mk_hash_root = mk_hash_map_full.compute_root().unwrap();
        let mk_hash_proof_compressed = mk_hash_map_compressed.compute_root().unwrap();

        assert_eq!(mk_hash_root, mk_hash_proof_compressed);
    }

    #[test]
    fn test_mk_hash_map_should_compute_and_verify_valid_proof() {
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
        let mk_hash_map_full = MKHashMap::new(merkle_tree_node_entries.as_slice()).unwrap();
        let mk_hash_proof = mk_hash_map_full
            .compute_proof(&mktree_nodes_to_certify)
            .unwrap();

        mk_hash_proof.verify().unwrap();

        let hash_proof_root = mk_hash_proof.compute_root();
        let hash_proof_root_expected = mk_hash_map_full.compute_root().unwrap();
        assert_eq!(hash_proof_root, hash_proof_root_expected);
    }

    #[test]
    fn test_mk_hash_map_should_compute_and_verify_valid_proof_recursively() {
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
            .map(|(range, mktree)| (range.to_owned(), MKHashMapNode::Tree(Rc::new(mktree))))
            .collect::<Vec<_>>()
            .chunks(10)
            .map(|entries| {
                (
                    entries
                        .iter()
                        .fold(BlockRange::new(0, 0), |mut acc, (range, _)| {
                            acc.try_add(range).unwrap()
                        }),
                    MKHashMapNode::HashMap(Rc::new(MKHashMap::new(entries).unwrap())),
                )
            })
            .collect::<Vec<_>>();

        let mk_hash_map_full = MKHashMap::new(merkle_tree_node_entries.as_slice()).unwrap();

        let mk_hash_proof = mk_hash_map_full
            .compute_proof(&mktree_nodes_to_certify)
            .unwrap();

        mk_hash_proof.verify().unwrap();

        let hash_proof_root = mk_hash_proof.compute_root();
        let hash_proof_root_expected = mk_hash_map_full.compute_root().unwrap();
        assert_eq!(hash_proof_root, hash_proof_root_expected);
    }
}
