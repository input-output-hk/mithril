use serde::{Deserialize, Serialize};

use crate::{
    MembershipDigest, StmResult,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeSnarkLeaf},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf>,
}

impl<D: MembershipDigest> AggregateVerificationKeyForSnark<D> {
    /// Get the Merkle tree batch commitment.
    pub(crate) fn get_merkle_tree_commitment(
        &self,
    ) -> MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf> {
        self.merkle_tree_commitment.clone()
    }

    /// Convert the aggregate verification key for Snark to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(self.merkle_tree_commitment.to_bytes());

        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let size = bytes.len();
        let merkle_tree_commitment = MerkleTreeCommitment::from_bytes(&bytes[0..size])?;
        Ok(Self {
            merkle_tree_commitment,
        })
    }
}
