use anyhow::{anyhow, Context};
use blake2::{Blake2s256, Digest};
use ckb_merkle_mountain_range::{
    MMRStoreReadOps, MMRStoreWriteOps, Merge, MerkleProof, Result as MMRResult, MMR,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
    ops::{Add, Deref},
    sync::{Arc, RwLock},
};

use crate::{StdError, StdResult};

/// Alias for a byte
type Bytes = Vec<u8>;

/// Alias for a Merkle tree leaf position
type MKTreeLeafPosition = u64;

/// A node of a Merkle tree
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Serialize, Deserialize)]
pub struct MKTreeNode {
    hash: Bytes,
}

impl MKTreeNode {
    /// MKTreeNode factory
    pub fn new(hash: Bytes) -> Self {
        Self { hash }
    }

    /// Create a MKTreeNode from a hex representation
    pub fn from_hex(hex: &str) -> StdResult<Self> {
        let hash = hex::decode(hex)?;
        Ok(Self { hash })
    }

    /// Create a hex representation of the MKTreeNode
    pub fn to_hex(&self) -> String {
        hex::encode(&self.hash)
    }
}

impl Deref for MKTreeNode {
    type Target = Bytes;

    fn deref(&self) -> &Self::Target {
        &self.hash
    }
}

impl From<String> for MKTreeNode {
    fn from(other: String) -> Self {
        Self {
            hash: other.as_str().into(),
        }
    }
}

impl From<&str> for MKTreeNode {
    fn from(other: &str) -> Self {
        Self {
            hash: other.as_bytes().to_vec(),
        }
    }
}

impl<S: MKTreeStorer> TryFrom<MKTree<S>> for MKTreeNode {
    type Error = StdError;
    fn try_from(other: MKTree<S>) -> Result<Self, Self::Error> {
        other.compute_root()
    }
}

impl<S: MKTreeStorer> TryFrom<&MKTree<S>> for MKTreeNode {
    type Error = StdError;
    fn try_from(other: &MKTree<S>) -> Result<Self, Self::Error> {
        other.compute_root()
    }
}

impl Display for MKTreeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.hash))
    }
}

impl Add for MKTreeNode {
    type Output = MKTreeNode;

    fn add(self, other: MKTreeNode) -> MKTreeNode {
        &self + &other
    }
}

impl Add for &MKTreeNode {
    type Output = MKTreeNode;

    fn add(self, other: &MKTreeNode) -> MKTreeNode {
        let mut hasher = Blake2s256::new();
        hasher.update(self.deref());
        hasher.update(other.deref());
        let hash_merge = hasher.finalize();
        MKTreeNode::new(hash_merge.to_vec())
    }
}

struct MergeMKTreeNode {}

impl Merge for MergeMKTreeNode {
    type Item = Arc<MKTreeNode>;

    fn merge(lhs: &Self::Item, rhs: &Self::Item) -> MMRResult<Self::Item> {
        Ok(Arc::new((**lhs).clone() + (**rhs).clone()))
    }
}

/// A Merkle proof
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct MKProof {
    inner_root: Arc<MKTreeNode>,
    inner_leaves: Vec<(MKTreeLeafPosition, Arc<MKTreeNode>)>,
    inner_proof_size: u64,
    inner_proof_items: Vec<Arc<MKTreeNode>>,
}

impl MKProof {
    /// Return a reference to its merkle root.
    pub fn root(&self) -> &MKTreeNode {
        &self.inner_root
    }

    /// Verification of a Merkle proof
    pub fn verify(&self) -> StdResult<()> {
        MerkleProof::<Arc<MKTreeNode>, MergeMKTreeNode>::new(
            self.inner_proof_size,
            self.inner_proof_items.clone(),
        )
        .verify(self.inner_root.to_owned(), self.inner_leaves.to_owned())?
        .then_some(())
        .ok_or(anyhow!("Invalid MKProof"))
    }

    /// Check if the proof contains the given leaves
    pub fn contains(&self, leaves: &[MKTreeNode]) -> StdResult<()> {
        leaves
            .iter()
            .all(|leaf| self.inner_leaves.iter().any(|(_, l)| l.deref() == leaf))
            .then_some(())
            .ok_or(anyhow!("Leaves not found in the MKProof"))
    }

    /// List the leaves of the proof
    pub fn leaves(&self) -> Vec<MKTreeNode> {
        self.inner_leaves
            .iter()
            .map(|(_, l)| (**l).clone())
            .collect::<Vec<_>>()
    }

    cfg_test_tools! {
        /// Build a [MKProof] based on the given leaves (*Test only*).
        pub fn from_leaves<T: Into<MKTreeNode> + Clone>(
            leaves: &[T],
        ) -> StdResult<MKProof> {
            Self::from_subset_of_leaves(leaves, leaves)
        }

        /// Build a [MKProof] based on the given leaves (*Test only*).
        pub fn from_subset_of_leaves<T: Into<MKTreeNode> + Clone>(
            leaves: &[T],
            leaves_to_verify: &[T],
        ) -> StdResult<MKProof> {
            let leaves = Self::list_to_mknode(leaves);
            let leaves_to_verify =
                Self::list_to_mknode(leaves_to_verify);

            let mktree =
                MKTree::<MKTreeStoreInMemory>::new(&leaves).with_context(|| "MKTree creation should not fail")?;
            mktree.compute_proof(&leaves_to_verify)
        }

        fn list_to_mknode<T: Into<MKTreeNode> + Clone>(hashes: &[T]) -> Vec<MKTreeNode> {
            hashes.iter().map(|h| h.clone().into()).collect()
        }
    }
}

impl From<MKProof> for MKTreeNode {
    fn from(other: MKProof) -> Self {
        other.root().to_owned()
    }
}

/// A Merkle tree store
#[derive(Clone)]
pub struct MKTreeStoreInMemoryProvider<T> {
    inner_store: Arc<RwLock<HashMap<u64, T>>>,
}

impl<T> MKTreeStoreInMemoryProvider<T> {
    fn new() -> Self {
        Self {
            inner_store: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

impl<T: Clone> MMRStoreReadOps<T> for MKTreeStoreInMemoryProvider<T> {
    fn get_elem(&self, pos: u64) -> MMRResult<Option<T>> {
        let inner_store = self.inner_store.read().unwrap();

        Ok((*inner_store).get(&pos).cloned())
    }
}

impl<T> MMRStoreWriteOps<T> for MKTreeStoreInMemoryProvider<T> {
    fn append(&mut self, pos: u64, elems: Vec<T>) -> MMRResult<()> {
        let mut inner_store = self.inner_store.write().unwrap();
        for (i, elem) in elems.into_iter().enumerate() {
            (*inner_store).insert(pos + i as u64, elem);
        }

        Ok(())
    }
}

impl<T> Default for MKTreeStoreInMemoryProvider<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A Merkle tree store
#[derive(Clone)]
pub struct MKTreeStoreInMemory {
    inner_leaves: Arc<RwLock<HashMap<Arc<MKTreeNode>, MKTreeLeafPosition>>>,
    inner_provider: MKTreeStoreInMemoryProvider<Arc<MKTreeNode>>,
}

impl MKTreeStoreInMemory {
    fn new() -> Self {
        Self {
            inner_provider: MKTreeStoreInMemoryProvider::<Arc<MKTreeNode>>::default(),
            inner_leaves: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

impl MMRStoreReadOps<Arc<MKTreeNode>> for MKTreeStoreInMemory {
    fn get_elem(&self, pos: u64) -> MMRResult<Option<Arc<MKTreeNode>>> {
        self.inner_provider.get_elem(pos)
    }
}

impl MMRStoreWriteOps<Arc<MKTreeNode>> for MKTreeStoreInMemory {
    fn append(&mut self, pos: u64, elems: Vec<Arc<MKTreeNode>>) -> MMRResult<()> {
        self.inner_provider.append(pos, elems)
    }
}

impl Default for MKTreeStoreInMemory {
    fn default() -> Self {
        Self::new()
    }
}

impl MKTreeLeafIndexer for MKTreeStoreInMemory {
    fn set_leaf_position(&self, pos: MKTreeLeafPosition, node: Arc<MKTreeNode>) {
        let mut inner_leaves = self.inner_leaves.write().unwrap();
        (*inner_leaves).insert(node, pos);
    }

    fn get_leaf_position(&self, node: &MKTreeNode) -> Option<MKTreeLeafPosition> {
        let inner_leaves = self.inner_leaves.read().unwrap();
        (*inner_leaves).get(node).cloned()
    }

    fn total_leaves(&self) -> usize {
        let inner_leaves = self.inner_leaves.read().unwrap();
        (*inner_leaves).len()
    }

    fn leaves(&self) -> Vec<MKTreeNode> {
        let inner_leaves = self.inner_leaves.read().unwrap();
        (*inner_leaves)
            .iter()
            .map(|(leaf, position)| (position, leaf))
            .collect::<BTreeMap<_, _>>()
            .into_values()
            .map(|leaf| (**leaf).clone())
            .collect()
    }
}

impl MKTreeStorer for MKTreeStoreInMemory {}

/// The Merkle tree storer trait
pub trait MKTreeStorer:
    Clone
    + Default
    + Send
    + Sync
    + MKTreeLeafIndexer
    + MMRStoreReadOps<Arc<MKTreeNode>>
    + MMRStoreWriteOps<Arc<MKTreeNode>>
{
}

/// The Merkle tree leaves indexer trait
pub trait MKTreeLeafIndexer {
    /// Get the position of the leaf in the Merkle tree
    fn set_leaf_position(&self, pos: MKTreeLeafPosition, leaf: Arc<MKTreeNode>) -> StdResult<()>;

    /// Get the position of the leaf in the Merkle tree
    fn get_leaf_position(&self, leaf: &MKTreeNode) -> Option<MKTreeLeafPosition>;

    /// Number of leaves in the Merkle tree
    fn total_leaves(&self) -> usize;

    /// List of leaves with their positions in the Merkle tree
    fn leaves(&self) -> Vec<MKTreeNode>;

    /// Check if the Merkle tree contains the given leaf
    fn contains_leaf(&self, leaf: &MKTreeNode) -> bool {
        self.get_leaf_position(leaf).is_some()
    }
}

/// A Merkle tree
pub struct MKTree<S: MKTreeStorer> {
    inner_tree: MMR<Arc<MKTreeNode>, MergeMKTreeNode, S>,
}

impl<S: MKTreeStorer> MKTree<S> {
    /// MKTree factory
    pub fn new<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<Self> {
        let mut inner_tree = MMR::<_, _, _>::new(0, S::default());
        for leaf in leaves {
            let leaf = Arc::new(leaf.to_owned().into());
            let inner_tree_position = inner_tree.push(leaf.clone())?;
            inner_tree
                .store()
                .set_leaf_position(inner_tree_position, leaf.clone())?;
        }
        inner_tree.commit()?;

        Ok(Self { inner_tree })
    }

    /// Append leaves to the Merkle tree
    pub fn append<T: Into<MKTreeNode> + Clone>(&mut self, leaves: &[T]) -> StdResult<()> {
        for leaf in leaves {
            let leaf = Arc::new(leaf.to_owned().into());
            let inner_tree_position = self.inner_tree.push(leaf.clone())?;
            self.inner_tree
                .store()
                .set_leaf_position(inner_tree_position, leaf.clone())?;
        }
        self.inner_tree.commit()?;

        Ok(())
    }

    /// Number of leaves in the Merkle tree
    pub fn total_leaves(&self) -> usize {
        self.inner_tree.store().total_leaves()
    }

    /// List of leaves with their positions in the Merkle tree
    pub fn leaves(&self) -> Vec<MKTreeNode> {
        self.inner_tree.store().leaves()
    }

    /// Check if the Merkle tree contains the given leaf
    pub fn contains(&self, leaf: &MKTreeNode) -> bool {
        self.inner_tree.store().contains_leaf(leaf)
    }

    /// Generate root of the Merkle tree
    pub fn compute_root(&self) -> StdResult<MKTreeNode> {
        Ok((*self
            .inner_tree
            .get_root()
            .with_context(|| "Could not compute Merkle Tree root")?)
        .clone())
    }

    /// Generate Merkle proof of memberships in the tree
    pub fn compute_proof(&self, leaves: &[MKTreeNode]) -> StdResult<MKProof> {
        let inner_leaves = leaves
            .iter()
            .map(|leaf| {
                if let Some(leaf_position) = self.inner_tree.store().get_leaf_position(leaf) {
                    Ok((leaf_position, Arc::new(leaf.to_owned())))
                } else {
                    Err(anyhow!("Leaf not found in the Merkle tree"))
                }
            })
            .collect::<StdResult<Vec<_>>>()?;
        let proof = self.inner_tree.gen_proof(
            inner_leaves
                .iter()
                .map(|(leaf_position, _leaf)| *leaf_position)
                .collect(),
        )?;
        return Ok(MKProof {
            inner_root: Arc::new(self.compute_root()?),
            inner_leaves,
            inner_proof_size: proof.mmr_size(),
            inner_proof_items: proof.proof_items().to_vec(),
        });
    }
}

impl<S: MKTreeStorer> Clone for MKTree<S> {
    fn clone(&self) -> Self {
        // Cloning should never fail so unwrap is safe
        Self::new(&self.leaves()).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn generate_leaves(total_leaves: usize) -> Vec<MKTreeNode> {
        (0..total_leaves)
            .map(|i| format!("test-{i}").into())
            .collect()
    }

    #[test]
    fn test_golden_merkle_root() {
        let leaves = vec!["golden-1", "golden-2", "golden-3", "golden-4", "golden-5"];
        let mktree =
            MKTree::<MKTreeStoreInMemory>::new(&leaves).expect("MKTree creation should not fail");
        let mkroot = mktree
            .compute_root()
            .expect("MKRoot generation should not fail");

        assert_eq!(
            "3bbced153528697ecde7345a22e50115306478353619411523e804f2323fd921",
            mkroot.to_hex()
        );
    }

    #[test]
    fn test_should_accept_valid_proof_generated_by_merkle_tree() {
        let leaves = generate_leaves(10);
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let proof =
            MKProof::from_leaves(leaves_to_verify).expect("MKProof generation should not fail");
        proof.verify().expect("The MKProof should be valid");
    }

    #[test]
    fn test_should_reject_invalid_proof_generated_by_merkle_tree() {
        let leaves = generate_leaves(10);
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let mut proof =
            MKProof::from_leaves(leaves_to_verify).expect("MKProof generation should not fail");
        proof.inner_root = Arc::new(leaves[1].to_owned());
        proof.verify().expect_err("The MKProof should be invalid");
    }

    #[test]
    fn test_should_list_leaves() {
        let leaves: Vec<MKTreeNode> = vec!["test-0".into(), "test-1".into(), "test-2".into()];
        let mktree =
            MKTree::<MKTreeStoreInMemory>::new(&leaves).expect("MKTree creation should not fail");
        let leaves_retrieved = mktree.leaves();

        assert_eq!(
            leaves.iter().collect::<Vec<_>>(),
            leaves_retrieved.iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_should_clone_and_compute_same_root() {
        let leaves = generate_leaves(10);
        let mktree =
            MKTree::<MKTreeStoreInMemory>::new(&leaves).expect("MKTree creation should not fail");
        let mktree_clone = mktree.clone();

        assert_eq!(
            mktree.compute_root().unwrap(),
            mktree_clone.compute_root().unwrap(),
        );
    }

    #[test]
    fn test_should_support_append_leaves() {
        let leaves = generate_leaves(10);
        let leaves_creation = &leaves[..9];
        let leaves_to_append = &leaves[9..];
        let mut mktree = MKTree::<MKTreeStoreInMemory>::new(leaves_creation)
            .expect("MKTree creation should not fail");
        mktree
            .append(leaves_to_append)
            .expect("MKTree append leaves should not fail");

        assert_eq!(10, mktree.total_leaves());
    }

    #[test]
    fn tree_node_from_to_string() {
        let expected_str = "my_string";
        let expected_string = expected_str.to_string();
        let node_str: MKTreeNode = expected_str.into();
        let node_string: MKTreeNode = expected_string.clone().into();

        assert_eq!(node_str.to_string(), expected_str);
        assert_eq!(node_string.to_string(), expected_string);
    }

    #[test]
    fn contains_leaves() {
        let mut leaves_to_verify = generate_leaves(10);
        let leaves_not_verified = leaves_to_verify.drain(3..6).collect::<Vec<_>>();
        let proof =
            MKProof::from_leaves(&leaves_to_verify).expect("MKProof generation should not fail");

        // contains everything
        proof.contains(&leaves_to_verify).unwrap();

        // contains subpart
        proof.contains(&leaves_to_verify[0..2]).unwrap();

        // don't contains all not verified
        proof.contains(&leaves_not_verified).unwrap_err();

        // don't contains subpart of not verified
        proof.contains(&leaves_not_verified[1..2]).unwrap_err();

        // fail if part verified and part unverified
        proof
            .contains(&[
                leaves_to_verify[2].to_owned(),
                leaves_not_verified[0].to_owned(),
            ])
            .unwrap_err();
    }

    #[test]
    fn list_leaves() {
        let leaves_to_verify = generate_leaves(10);
        let proof =
            MKProof::from_leaves(&leaves_to_verify).expect("MKProof generation should not fail");

        let proof_leaves = proof.leaves();
        assert_eq!(proof_leaves, leaves_to_verify);
    }
}
