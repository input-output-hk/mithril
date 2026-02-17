use serde::{Deserialize, Serialize};

use crate::{
    MembershipDigest,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeSnarkLeaf},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf>,
}
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl<D: MembershipDigest> AggregateVerificationKeyForSnark<D> {
    /// Get the Merkle tree commitment.
    pub(crate) fn get_merkle_tree_commitment(
        &self,
    ) -> MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf> {
        self.merkle_tree_commitment.clone()
    }
}
