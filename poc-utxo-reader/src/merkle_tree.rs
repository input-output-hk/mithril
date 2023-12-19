use anyhow::anyhow;
use blake2::{Blake2s256, Digest};
use ckb_merkle_mountain_range::{util::MemStore, Merge, MerkleProof, Result as MMRResult, MMR};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, ops::Deref};

use crate::errors::{StdError, StdResult};

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

// A Merkle proof
#[derive(Serialize, Deserialize)]
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

/// A Merkle tree
pub struct MKTree<'a> {
    inner_leaves: HashMap<&'a MKTreeNode, MKTreeLeafPosition>,
    inner_tree: MMR<MKTreeNode, MergeMKTreeNode, &'a MemStore<MKTreeNode>>,
}

impl<'a> MKTree<'a> {
    /// MKTree factory
    pub fn new(leaves: &'a [MKTreeNode], store: &'a MemStore<MKTreeNode>) -> StdResult<Self> {
        let mut inner_tree =
            MMR::<MKTreeNode, MergeMKTreeNode, &MemStore<MKTreeNode>>::new(0, store);
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
    use ckb_merkle_mountain_range::util::MemStore;

    use super::MKTree;

    #[test]
    fn test_should_accept_valid_proof_generated_by_merkle_tree() {
        let total_leaves = 100000;
        let leaves = (0..total_leaves)
            .map(|i| format!("test-{i}").into())
            .collect::<Vec<_>>();
        let store = MemStore::default();
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
        let store = MemStore::default();
        let mktree = MKTree::new(&leaves, &store).expect("MKTree creation should not fail");
        let leaves_to_verify = &[leaves[0].to_owned(), leaves[3].to_owned()];
        let mut proof = mktree
            .compute_proof(leaves_to_verify)
            .expect("MKProof generation should not fail");
        proof.inner_root = leaves[10].to_owned();
        proof.verify().expect_err("The MKProof should be invalid");
    }
}
