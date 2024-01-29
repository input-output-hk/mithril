use anyhow::anyhow;
use blake2::{Blake2s256, Digest};
use ckb_merkle_mountain_range::{util::MemStore, Merge, MerkleProof, Result as MMRResult, MMR};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, ops::Deref};

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

impl TryFrom<MKTree<'_>> for MKTreeNode {
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
    type Item = MKTreeNode;

    fn merge(lhs: &Self::Item, rhs: &Self::Item) -> MMRResult<Self::Item> {
        let mut hasher = Blake2s256::new();
        hasher.update(lhs.deref());
        hasher.update(rhs.deref());
        let hash_merge = hasher.finalize();

        Ok(Self::Item::new(hash_merge.to_vec()))
    }
}

/// A Merkle proof
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub struct MKProof {
    inner_root: MKTreeNode,
    inner_leaves: Vec<(MKTreeLeafPosition, MKTreeNode)>,
    inner_proof_size: u64,
    inner_proof_items: Vec<MKTreeNode>,
}

impl MKProof {
    /// Verification of a Merkle proof
    pub fn verify(&self) -> StdResult<()> {
        MerkleProof::<MKTreeNode, MergeMKTreeNode>::new(
            self.inner_proof_size,
            self.inner_proof_items.clone(),
        )
        .verify(self.inner_root.to_owned(), self.inner_leaves.to_owned())?
        .then_some(())
        .ok_or(anyhow!("Invalid MKProof"))
    }
}

/// A Merkle tree store
pub type MKTreeStore = MemStore<MKTreeNode>;

/// A Merkle tree
pub struct MKTree<'a> {
    inner_leaves: HashMap<&'a MKTreeNode, MKTreeLeafPosition>,
    inner_tree: MMR<MKTreeNode, MergeMKTreeNode, &'a MKTreeStore>,
}

impl<'a> MKTree<'a> {
    /// MKTree factory
    pub fn new(leaves: &'a [MKTreeNode], store: &'a MKTreeStore) -> StdResult<Self> {
        let mut inner_tree = MMR::<MKTreeNode, MergeMKTreeNode, &MKTreeStore>::new(0, store);
        let mut inner_leaves = HashMap::new();
        for leaf in leaves {
            let inner_tree_position = inner_tree.push(leaf.to_owned())?;
            inner_leaves.insert(leaf, inner_tree_position);
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
        Ok(self.inner_tree.get_root()?)
    }

    /// Generate Merkle proof of memberships in the tree
    pub fn compute_proof(&self, leaves: &[MKTreeNode]) -> StdResult<MKProof> {
        let inner_leaves = leaves
            .iter()
            .map(|leaf| {
                if let Some(leaf_position) = self.inner_leaves.get(leaf) {
                    Ok((*leaf_position, leaf.to_owned()))
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
            inner_root: self.compute_root()?,
            inner_leaves,
            inner_proof_size: proof.mmr_size(),
            inner_proof_items: proof.proof_items().to_vec(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_golden_merkle_root() {
        let leaves = vec!["golden-1", "golden-2", "golden-3", "golden-4", "golden-5"];
        let leaves: Vec<MKTreeNode> = leaves.into_iter().map(|l| l.into()).collect();
        let store = MKTreeStore::default();
        let mktree = MKTree::new(&leaves, &store).expect("MKTree creation should not fail");
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
        let total_leaves = 100000;
        let leaves = (0..total_leaves)
            .map(|i| format!("test-{i}").into())
            .collect::<Vec<_>>();
        let store = MKTreeStore::default();
        let mktree = MKTree::new(&leaves, &store).expect("MKTree creation should not fail");
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let proof = mktree
            .compute_proof(leaves_to_verify)
            .expect("MKProof generation should not fail");
        proof.verify().expect("The MKProof should be valid");
    }

    #[test]
    fn test_should_reject_invalid_proof_generated_by_merkle_tree() {
        let total_leaves = 100000;
        let leaves = (0..total_leaves)
            .map(|i| format!("test-{i}").into())
            .collect::<Vec<_>>();
        let store = MKTreeStore::default();
        let mktree = MKTree::new(&leaves, &store).expect("MKTree creation should not fail");
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let mut proof = mktree
            .compute_proof(leaves_to_verify)
            .expect("MKProof generation should not fail");
        proof.inner_root = leaves[10].to_owned();
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
}
