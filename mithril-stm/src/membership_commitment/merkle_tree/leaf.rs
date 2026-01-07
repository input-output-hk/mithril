use std::cmp::Ordering;

use serde::{Deserialize, Serialize};

use crate::{Stake, signature_scheme::BlsVerificationKey};

#[cfg(feature = "future_snark")]
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
use crate::StmResult;

#[cfg(feature = "future_snark")]
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
use super::MerkleTreeError;

/// Trait implemented to be used as a Merkle tree leaf.
pub trait MerkleTreeLeaf: Clone + Send + Sync + Copy {
    /// Converts the Merkle tree leaf to a bytes representation used internally by the Merkle tree
    fn as_bytes_for_merkle_tree(&self) -> Vec<u8>;
}

/// The values that are committed in the Merkle Tree for `ConcatenationProof`.
/// Namely, a verified `BlsVerificationKey` and its corresponding stake.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct MerkleTreeConcatenationLeaf(pub BlsVerificationKey, pub Stake);

impl MerkleTreeLeaf for MerkleTreeConcatenationLeaf {
    fn as_bytes_for_merkle_tree(&self) -> Vec<u8> {
        self.to_bytes()
    }
}

impl MerkleTreeConcatenationLeaf {
    fn to_bytes(self) -> Vec<u8> {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result.to_vec()
    }

    #[cfg(feature = "future_snark")]
    // TODO: remove this allow dead_code directive when function is called or future_snark is activated
    #[allow(dead_code)]
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let pk = BlsVerificationKey::from_bytes(bytes)
            .map_err(|_| MerkleTreeError::SerializationError)?;
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[96..]);
        let stake = Stake::from_be_bytes(u64_bytes);
        Ok(MerkleTreeConcatenationLeaf(pk, stake))
    }
}

impl From<MerkleTreeConcatenationLeaf> for (BlsVerificationKey, Stake) {
    fn from(leaf: MerkleTreeConcatenationLeaf) -> (BlsVerificationKey, Stake) {
        (leaf.0, leaf.1)
    }
}

impl PartialOrd for MerkleTreeConcatenationLeaf {
    /// Ordering of MT Values.
    ///
    /// First we order by stake, then by key. By having this ordering,
    /// we have the players with higher stake close together,
    /// meaning that the probability of having several signatures in the same side of the tree, is higher.
    /// This allows us to produce a more efficient batch opening of the merkle tree.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for MerkleTreeConcatenationLeaf {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1).then(self.0.cmp(&other.0))
    }
}
