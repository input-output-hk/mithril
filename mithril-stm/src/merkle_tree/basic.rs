use crate::error::MerkleTreeError;
use crate::merkle_tree::leaf::MTLeaf;
use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

/// Path of hashes from root to leaf in a Merkle Tree.
/// Contains all hashes on the path, and the index of the leaf.
/// Used to verify that signatures come from eligible signers.
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Path<D: Digest> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) index: usize,
    hasher: PhantomData<D>,
}

impl<D: Digest + FixedOutput> Path<D> {
    pub(crate) fn new(values: Vec<Vec<u8>>, index: usize) -> Self {
        Self {
            values,
            index,
            hasher: Default::default(),
        }
    }

    /// Convert to bytes
    /// # Layout
    /// * Index representing the position in the Merkle Tree
    /// * Size of the Path
    /// * Path of hashes
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut output = Vec::new();
        output.extend_from_slice(&u64::try_from(self.index).unwrap().to_be_bytes());
        output.extend_from_slice(&u64::try_from(self.values.len()).unwrap().to_be_bytes());
        for value in &self.values {
            output.extend_from_slice(value)
        }

        output
    }

    /// Extract a `Path` from a byte slice.
    /// # Error
    /// This function fails if the bytes cannot retrieve path.
    pub fn from_bytes(bytes: &[u8]) -> Result<Path<D>, MerkleTreeError<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let index = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        u64_bytes.copy_from_slice(&bytes[8..16]);
        let len = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let mut values = Vec::with_capacity(len);
        for i in 0..len {
            values.push(
                bytes[16 + i * <D as Digest>::output_size()
                    ..16 + (i + 1) * <D as Digest>::output_size()]
                    .to_vec(),
            );
        }

        Ok(Path {
            values,
            index,
            hasher: PhantomData,
        })
    }
}

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
    pub fn check(&self, val: &MTLeaf, proof: &Path<D>) -> Result<(), MerkleTreeError<D>> where D: FixedOutput + Clone, {
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
        Err(MerkleTreeError::PathInvalid(proof.clone()))
    }

    /// Serializes the Merkle Tree commitment together with a message in a single vector of bytes.
    /// Outputs `msg || self` as a vector of bytes.
    pub fn concat_with_msg(&self, msg: &[u8]) -> Vec<u8>
    where
        D: Digest,
    {
        let mut msgp = msg.to_vec();
        let mut bytes = self.root.clone();
        msgp.append(&mut bytes);

        msgp
    }
}
