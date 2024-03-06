use anyhow::anyhow;
#[cfg(any(test, feature = "test_tools"))]
use anyhow::Context;
use blake2::{Blake2s256, Digest};
use ckb_merkle_mountain_range::{
    util::MemStore, MMRStoreReadOps, MMRStoreWriteOps, Merge, MerkleProof, Result as MMRResult, MMR,
};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, ops::Deref, sync::Arc};

use crate::{StdError, StdResult};

/// Alias for a byte
type Bytes = Vec<u8>;

/// Alias for a Merkle tree leaf position
type MKTreeLeafPosition = u64;

/// A node of a Merkle tree
#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize, Deserialize)]
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

impl TryFrom<MKTree> for MKTreeNode {
    type Error = StdError;
    fn try_from(other: MKTree) -> Result<Self, Self::Error> {
        other.compute_root()
    }
}

impl ToString for MKTreeNode {
    fn to_string(&self) -> String {
        String::from_utf8_lossy(&self.hash).to_string()
    }
}

struct MergeMKTreeNode {}

impl Merge for MergeMKTreeNode {
    type Item = Arc<MKTreeNode>;

    fn merge(lhs: &Self::Item, rhs: &Self::Item) -> MMRResult<Self::Item> {
        let mut hasher = Blake2s256::new();
        hasher.update(Arc::unwrap_or_clone(lhs.to_owned()).deref());
        hasher.update(Arc::unwrap_or_clone(rhs.to_owned()).deref());
        let hash_merge = hasher.finalize();

        Ok(Arc::new(MKTreeNode::new(hash_merge.to_vec())))
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
            .all(|leaf| {
                self.inner_leaves
                    .iter()
                    .any(|(_, l)| &Arc::unwrap_or_clone(l.to_owned()) == leaf)
            })
            .then_some(())
            .ok_or(anyhow!("Leaves not found in the MKProof"))
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
                MKTree::new(&leaves).with_context(|| "MKTree creation should not fail")?;
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
pub struct MKTreeStore<T> {
    inner_store: MemStore<T>,
}

impl<T> MKTreeStore<T> {
    fn new() -> Self {
        Self {
            inner_store: MemStore::<T>::default(),
        }
    }
}

impl<T: Clone> MMRStoreReadOps<T> for MKTreeStore<T> {
    fn get_elem(&self, pos: u64) -> MMRResult<Option<T>> {
        let inner_store = &self.inner_store;
        inner_store.get_elem(pos)
    }
}

impl<T> MMRStoreWriteOps<T> for MKTreeStore<T> {
    fn append(&mut self, pos: u64, elems: Vec<T>) -> MMRResult<()> {
        let mut inner_store = &self.inner_store;
        inner_store.append(pos, elems)
    }
}

impl<T> Default for MKTreeStore<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A Merkle tree
pub struct MKTree {
    inner_leaves: HashMap<Arc<MKTreeNode>, MKTreeLeafPosition>,
    inner_tree: MMR<Arc<MKTreeNode>, MergeMKTreeNode, MKTreeStore<Arc<MKTreeNode>>>,
}

impl MKTree {
    /// MKTree factory
    pub fn new<T: Into<MKTreeNode> + Clone>(leaves: &[T]) -> StdResult<Self> {
        let mut inner_tree = MMR::<_, _, _>::new(0, MKTreeStore::default());
        let mut inner_leaves = HashMap::new();
        for leaf in leaves {
            let leaf = Arc::new(leaf.to_owned().into());
            let inner_tree_position = inner_tree.push(leaf.clone())?;
            inner_leaves.insert(leaf.clone(), inner_tree_position);
        }
        inner_tree.commit()?;

        Ok(Self {
            inner_leaves,
            inner_tree,
        })
    }

    /// Number of leaves in the Merkle tree
    pub fn total_leaves(&self) -> usize {
        self.inner_leaves.len()
    }

    /// Generate root of the Merkle tree
    pub fn compute_root(&self) -> StdResult<MKTreeNode> {
        Ok(Arc::unwrap_or_clone(self.inner_tree.get_root()?))
    }

    /// Generate Merkle proof of memberships in the tree
    pub fn compute_proof(&self, leaves: &[MKTreeNode]) -> StdResult<MKProof> {
        let inner_leaves = leaves
            .iter()
            .map(|leaf| {
                if let Some(leaf_position) = self.inner_leaves.get(leaf) {
                    Ok((*leaf_position, Arc::new(leaf.to_owned())))
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
        let mktree = MKTree::new(&leaves).expect("MKTree creation should not fail");
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
        let leaves = generate_leaves(100000);
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let proof =
            MKProof::from_leaves(leaves_to_verify).expect("MKProof generation should not fail");
        proof.verify().expect("The MKProof should be valid");
    }

    #[test]
    fn test_should_reject_invalid_proof_generated_by_merkle_tree() {
        let leaves = generate_leaves(100000);
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let mut proof =
            MKProof::from_leaves(leaves_to_verify).expect("MKProof generation should not fail");
        proof.inner_root = Arc::new(leaves[10].to_owned());
        proof.verify().expect_err("The MKProof should be invalid");
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
}
