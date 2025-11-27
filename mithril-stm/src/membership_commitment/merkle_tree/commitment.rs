use anyhow::{Context, anyhow};
use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

use super::{MerkleBatchPath, MerklePath, MerkleTreeLeaf, parent, sibling};
use crate::{MerkleTreeError, StmResult};

/// `MerkleTree` commitment.
/// This structure differs from `MerkleTree` in that it does not contain all elements, which are not always necessary.
/// Instead, it only contains the root of the tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTreeCommitment<D: Digest> {
    /// Root of the merkle commitment.
    pub root: Vec<u8>,
    hasher: PhantomData<D>,
}

impl<D: Digest + FixedOutput> MerkleTreeCommitment<D> {
    pub(crate) fn new(root: Vec<u8>) -> Self {
        MerkleTreeCommitment {
            root,
            hasher: PhantomData,
        }
    }

    /// Check an inclusion proof that `val` is part of the tree by traveling the whole path until the root.
    /// # Error
    /// If the merkle tree path is invalid, then the function fails.
    pub(crate) fn verify_leaf_membership_from_path(
        &self,
        val: &MerkleTreeLeaf,
        proof: &MerklePath<D>,
    ) -> StmResult<()>
    where
        D: FixedOutput + Clone,
    {
        let mut idx = proof.index;

        let mut h = D::digest(val.to_bytes()).to_vec();
        for p in &proof.values {
            if (idx & 0b1) == 0 {
                h = D::new().chain_update(h).chain_update(p).finalize().to_vec();
            } else {
                h = D::new().chain_update(p).chain_update(h).finalize().to_vec();
            }
            idx >>= 1;
        }

        if h == self.root {
            return Ok(());
        }
        Err(anyhow!(MerkleTreeError::PathInvalid(proof.to_bytes())))
    }

    /// Check an inclusion proof that `val` is part of the tree by traveling the whole path until the root.
    /// # Error
    /// If the merkle tree path is invalid, then the function fails.
    #[deprecated(
        since = "0.5.0",
        note = "Use `verify_leaf_membership_from_path` instead"
    )]
    pub fn check(&self, val: &MerkleTreeLeaf, proof: &MerklePath<D>) -> StmResult<()>
    where
        D: FixedOutput + Clone,
    {
        Self::verify_leaf_membership_from_path(self, val, proof)
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    fn concatenate_with_message(&self, msg: &[u8]) -> Vec<u8>
    where
        D: Digest,
    {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    #[deprecated(since = "0.5.0", note = "Use `concatenate_with_message` instead")]
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8>
    where
        D: Digest,
    {
        Self::concatenate_with_message(self, msg)
    }

    /// Convert to bytes
    /// # Layout
    /// * Root of the Merkle commitment
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&self.root);
        output
    }

    /// Extract a `MerkleTreeCommitment` from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<MerkleTreeCommitment<D>> {
        let root = bytes.to_vec();

        Ok(Self {
            root,
            hasher: PhantomData,
        })
    }
}

/// Batch compatible `MerkleTree` commitment .
/// This structure differs from `MerkleTreeCommitment` in that it stores the number of leaves in the tree
/// as well as the root of the tree.
/// Number of leaves is required by the batch path generation/verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTreeBatchCommitment<D: Digest> {
    /// Root of the merkle commitment.
    pub root: Vec<u8>,
    nr_leaves: usize,
    hasher: PhantomData<D>,
}

impl<D: Digest + FixedOutput> MerkleTreeBatchCommitment<D> {
    pub(crate) fn new(root: Vec<u8>, nr_leaves: usize) -> Self {
        Self {
            root,
            nr_leaves,
            hasher: Default::default(),
        }
    }

    #[cfg(test)]
    /// Used in property test of `tree`: `test_bytes_tree_commitment_batch_compat`
    pub(crate) fn get_number_of_leaves(&self) -> usize {
        self.nr_leaves
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    // todo: Do we need to concat msg to whole commitment (nr_leaves and root) or just the root?
    pub(crate) fn concatenate_with_message(&self, msg: &[u8]) -> Vec<u8> {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    // todo: Do we need to concat msg to whole commitment (nr_leaves and root) or just the root?
    #[deprecated(since = "0.5.0", note = "Use `concatenate_with_message` instead")]
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8> {
        Self::concatenate_with_message(self, msg)
    }

    /// Check a proof of a batched opening. The indices must be ordered.
    ///
    /// # Error
    /// Returns an error if the proof is invalid.
    // todo: Update doc.
    // todo: Simplify the algorithm.
    // todo: Maybe we want more granular errors, rather than only `BatchPathInvalid`
    pub(crate) fn verify_leaves_membership_from_batch_path(
        &self,
        batch_val: &[MerkleTreeLeaf],
        proof: &MerkleBatchPath<D>,
    ) -> StmResult<()>
    where
        D: FixedOutput + Clone,
    {
        if batch_val.len() != proof.indices.len() {
            return Err(anyhow!(MerkleTreeError::BatchPathInvalid(proof.to_bytes())));
        }
        let mut ordered_indices: Vec<usize> = proof.indices.clone();
        ordered_indices.sort_unstable();

        if ordered_indices != proof.indices {
            return Err(anyhow!(MerkleTreeError::BatchPathInvalid(proof.to_bytes())));
        }

        let nr_nodes = self.nr_leaves + self.nr_leaves.next_power_of_two() - 1;

        ordered_indices = ordered_indices
            .into_iter()
            .map(|i| i + self.nr_leaves.next_power_of_two() - 1)
            .collect();

        let mut idx = ordered_indices[0];
        // First we need to hash the leave values
        let mut leaves: Vec<Vec<u8>> = batch_val
            .iter()
            .map(|val| D::digest(val.to_bytes()).to_vec())
            .collect();

        let mut values = proof.values.clone();

        while idx > 0 {
            let mut new_hashes = Vec::with_capacity(ordered_indices.len());
            let mut new_indices = Vec::with_capacity(ordered_indices.len());
            let mut i = 0;
            idx = parent(idx);
            while i < ordered_indices.len() {
                new_indices.push(parent(ordered_indices[i]));
                if ordered_indices[i] & 1 == 0 {
                    new_hashes.push(
                        D::new()
                            .chain(
                                values
                                    .first()
                                    .ok_or(MerkleTreeError::SerializationError)
                                    .with_context(|| {
                                        format!("Could not verify leave membership from batch path for idx = {} and ordered_indices[{}]", idx, i)
                                    })?,
                            )
                            .chain(&leaves[i])
                            .finalize()
                            .to_vec(),
                    );
                    values.remove(0);
                } else {
                    let sibling = sibling(ordered_indices[i]);
                    if i < ordered_indices.len() - 1 && ordered_indices[i + 1] == sibling {
                        new_hashes.push(
                            D::new().chain(&leaves[i]).chain(&leaves[i + 1]).finalize().to_vec(),
                        );
                        i += 1;
                    } else if sibling < nr_nodes {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(
                                    values
                                        .first()
                                        .ok_or(MerkleTreeError::SerializationError)
                                        .with_context(|| {
                                            format!(
                                                "Could not verify leave membership from batch path for idx = {} where sibling < nr_nodes", idx
                                            )
                                        })?,
                                )
                                .finalize()
                                .to_vec(),
                        );
                        values.remove(0);
                    } else {
                        new_hashes.push(
                            D::new().chain(&leaves[i]).chain(D::digest([0u8])).finalize().to_vec(),
                        );
                    }
                }
                i += 1;
            }
            leaves.clone_from(&new_hashes);
            ordered_indices.clone_from(&new_indices);
        }

        if leaves.len() == 1 && leaves[0] == self.root {
            return Ok(());
        }

        Err(anyhow!(MerkleTreeError::BatchPathInvalid(proof.to_bytes())))
    }

    /// Check a proof of a batched opening. The indices must be ordered.
    ///
    /// # Error
    /// Returns an error if the proof is invalid.
    // todo: Update doc.
    // todo: Simplify the algorithm.
    // todo: Maybe we want more granular errors, rather than only `BatchPathInvalid`
    #[deprecated(
        since = "0.5.0",
        note = "Use `verify_leaves_membership_from_batch_path` instead"
    )]
    pub fn check(&self, batch_val: &[MerkleTreeLeaf], proof: &MerkleBatchPath<D>) -> StmResult<()>
    where
        D: FixedOutput + Clone,
    {
        Self::verify_leaves_membership_from_batch_path(self, batch_val, proof)
    }

    /// Convert to bytes
    /// * Number of leaves as u64
    /// * Root of the Merkle commitment as bytes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&u64::try_from(self.nr_leaves).unwrap().to_be_bytes());
        output.extend_from_slice(&self.root);

        output
    }

    /// Extract a `MerkleTreeBatchCommitment` from a byte slice.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<MerkleTreeBatchCommitment<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(MerkleTreeError::SerializationError)?);
        let nr_leaves = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let mut root = Vec::new();
        root.extend_from_slice(bytes.get(8..).ok_or(MerkleTreeError::SerializationError)?);

        Ok(MerkleTreeBatchCommitment {
            root,
            nr_leaves,
            hasher: PhantomData,
        })
    }
}

impl<D: Digest> PartialEq for MerkleTreeBatchCommitment<D> {
    fn eq(&self, other: &Self) -> bool {
        self.root == other.root && self.nr_leaves == other.nr_leaves
    }
}

impl<D: Digest> Eq for MerkleTreeBatchCommitment<D> {}
