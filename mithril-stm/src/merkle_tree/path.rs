use std::marker::PhantomData;

use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::error::MerkleTreeError;

/// Path of hashes from root to leaf in a Merkle Tree.
/// Contains all hashes on the path, and the index of the leaf.
/// Used to verify that signatures come from eligible signers.
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MerklePath<D: Digest> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) index: usize,
    hasher: PhantomData<D>,
}

impl<D: Digest + FixedOutput> MerklePath<D> {
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
    pub fn from_bytes(bytes: &[u8]) -> Result<MerklePath<D>, MerkleTreeError<D>> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(..8).ok_or(MerkleTreeError::SerializationError)?);
        let index = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        u64_bytes.copy_from_slice(bytes.get(8..16).ok_or(MerkleTreeError::SerializationError)?);
        let len = usize::try_from(u64::from_be_bytes(u64_bytes))
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let mut values = Vec::with_capacity(len);
        for i in 0..len {
            values.push(
                bytes
                    .get(
                        16 + i * <D as Digest>::output_size()
                            ..16 + (i + 1) * <D as Digest>::output_size(),
                    )
                    .ok_or(MerkleTreeError::SerializationError)?
                    .to_vec(),
            );
        }

        Ok(MerklePath {
            values,
            index,
            hasher: PhantomData,
        })
    }
}

/// Path of hashes for a batch of indices.
/// Contains the hashes and the corresponding merkle tree indices of given batch.
/// Used to verify the signatures are issued by the registered signers.
#[derive(Default, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MerkleBatchPath<D: Digest + FixedOutput> {
    pub(crate) values: Vec<Vec<u8>>,
    pub(crate) indices: Vec<usize>,
    pub(crate) hasher: PhantomData<D>,
}

impl<D: Digest + FixedOutput> MerkleBatchPath<D> {
    pub(crate) fn new(values: Vec<Vec<u8>>, indices: Vec<usize>) -> MerkleBatchPath<D> {
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

        Ok(MerkleBatchPath {
            values,
            indices,
            hasher: PhantomData,
        })
    }
}
