use std::cmp::Ordering;

use serde::{Deserialize, Serialize};

use crate::signature_scheme::BlsVerificationKey;
use crate::{MerkleTreeError, StmResult};
use crate::{Stake, VerificationKey};

/// The values that are committed in the Merkle Tree.
/// Namely, a verified `VerificationKey` and its corresponding stake.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct MerkleTreeLeaf(pub BlsVerificationKey, pub Stake);

impl MerkleTreeLeaf {
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let pk =
            VerificationKey::from_bytes(bytes).map_err(|_| MerkleTreeError::SerializationError)?;
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[96..]);
        let stake = Stake::from_be_bytes(u64_bytes);
        Ok(MerkleTreeLeaf(pk, stake))
    }
    pub(crate) fn to_bytes(self) -> [u8; 104] {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result
    }
}

impl From<MerkleTreeLeaf> for (VerificationKey, Stake) {
    fn from(leaf: MerkleTreeLeaf) -> (VerificationKey, Stake) {
        (leaf.0, leaf.1)
    }
}

impl PartialOrd for MerkleTreeLeaf {
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

impl Ord for MerkleTreeLeaf {
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1).then(self.0.cmp(&other.0))
    }
}
