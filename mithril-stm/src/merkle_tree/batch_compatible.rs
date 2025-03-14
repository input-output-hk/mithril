use crate::error::MerkleTreeError;
use crate::merkle_tree::leaf::MTLeaf;
use crate::merkle_tree::tree::{parent, sibling};
use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

/// Path of hashes for a batch of indices.
/// Contains the hashes and the corresponding merkle tree indices of given batch.
/// Used to verify the signatures are issued by the registered signers.
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct BatchPath<D: Digest + FixedOutput> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) indices: Vec<usize>,
    pub(crate) hasher: PhantomData<D>,
}

impl<D: Digest + FixedOutput> BatchPath<D> {
    pub(crate) fn new(values: Vec<Vec<u8>>, indices: Vec<usize>) -> BatchPath<D> {
        Self {
            values,
            indices,
            hasher: Default::default(),
        }
    }

    /// Convert the `BatchPath` into byte representation.
    ///
    /// # Layout
    /// The layout of a `BatchPath` is
    /// * Length of values
    /// * Length of indices
    /// * Values
    /// * Indices
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        let len_v = self.values.len();
        let len_i = self.indices.len();

        output.extend_from_slice(&u64::try_from(len_v).unwrap().to_be_bytes());
        output.extend_from_slice(&u64::try_from(len_i).unwrap().to_be_bytes());

        for value in &self.values {
            output.extend_from_slice(value.as_slice())
        }

        for &index in &self.indices {
            output.extend_from_slice(&u64::try_from(index).unwrap().to_be_bytes());
        }
        output
    }

    /// Try to convert a byte string into a `BatchPath`.
    // todo: We should not panic if the size of the slice is invalid (I believe `bytes[offset + i * 8..offset + (i + 1) * 8]` will panic if bytes is not large enough.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MerkleTreeError<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let len_v = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;

        u64_bytes.copy_from_slice(&bytes[8..16]);
        let len_i = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;

        let mut values = Vec::with_capacity(len_v);
        for i in 0..len_v {
            values.push(
                bytes[16 + i * <D as Digest>::output_size()
                    ..16 + (i + 1) * <D as Digest>::output_size()]
                    .to_vec(),
            );
        }
        let offset = 16 + len_v * <D as Digest>::output_size();

        let mut indices = Vec::with_capacity(len_v);
        for i in 0..len_i {
            u64_bytes.copy_from_slice(&bytes[offset + i * 8..offset + (i + 1) * 8]);
            indices.push(
                usize::try_from(u64::from_be_bytes(u64_bytes))
                    .map_err(|_| MerkleTreeError::SerializationError)?,
            );
        }

        Ok(BatchPath {
            values,
            indices,
            hasher: PhantomData,
        })
    }
}

/// Batch compatible `MerkleTree` commitment .
/// This structure differs from `MerkleTreeCommitment` in that it stores the number of leaves in the tree
/// as well as the root of the tree.
/// Number of leaves is required by the batch path generation/verification.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTreeCommitmentBatchCompat<D: Digest> {
    /// Root of the merkle commitment.
    pub root: Vec<u8>,
    nr_leaves: usize,
    hasher: PhantomData<D>,
}

impl<D: Digest> MerkleTreeCommitmentBatchCompat<D> {
    pub(crate) fn new(root: Vec<u8>, nr_leaves: usize) -> Self {
        Self {
            root,
            nr_leaves,
            hasher: Default::default(),
        }
    }

    #[allow(dead_code)]
    /// Used in property test of `tree`: `test_bytes_tree_commitment_batch_compat`
    pub(crate) fn get_nr_leaves(&self) -> usize {
        self.nr_leaves
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    // todo: Do we need to concat msg to whole commitment (nr_leaves and root) or just the root?
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8>
    {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }

    /// Check a proof of a batched opening. The indices must be ordered.
    ///
    /// # Error
    /// Returns an error if the proof is invalid.
    // todo: Update doc.
    // todo: Simplify the algorithm.
    // todo: Maybe we want more granular errors, rather than only `BatchPathInvalid`
    pub fn check(
        &self,
        batch_val: &[MTLeaf],
        proof: &BatchPath<D>,
    ) -> Result<(), MerkleTreeError<D>>
    where
        D: FixedOutput + Clone,
    {
        if batch_val.len() != proof.indices.len() {
            return Err(MerkleTreeError::BatchPathInvalid(proof.clone()));
        }
        let mut ordered_indices: Vec<usize> = proof.indices.clone();
        ordered_indices.sort_unstable();

        if ordered_indices != proof.indices {
            return Err(MerkleTreeError::BatchPathInvalid(proof.clone()));
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
                            .chain(values.first().ok_or(MerkleTreeError::SerializationError)?)
                            .chain(&leaves[i])
                            .finalize()
                            .to_vec(),
                    );
                    values.remove(0);
                } else {
                    let sibling = sibling(ordered_indices[i]);
                    if i < ordered_indices.len() - 1 && ordered_indices[i + 1] == sibling {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(&leaves[i + 1])
                                .finalize()
                                .to_vec(),
                        );
                        i += 1;
                    } else if sibling < nr_nodes {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(values.first().ok_or(MerkleTreeError::SerializationError)?)
                                .finalize()
                                .to_vec(),
                        );
                        values.remove(0);
                    } else {
                        new_hashes.push(
                            D::new()
                                .chain(&leaves[i])
                                .chain(D::digest([0u8]))
                                .finalize()
                                .to_vec(),
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

        Err(MerkleTreeError::BatchPathInvalid(proof.clone()))
    }
}

impl<D: Digest> PartialEq for MerkleTreeCommitmentBatchCompat<D> {
    fn eq(&self, other: &Self) -> bool {
        self.root == other.root && self.nr_leaves == other.nr_leaves
    }
}

impl<D: Digest> Eq for MerkleTreeCommitmentBatchCompat<D> {}
