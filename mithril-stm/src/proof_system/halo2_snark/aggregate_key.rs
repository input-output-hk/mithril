use serde::{Deserialize, Serialize};

use crate::{
    MembershipDigest, Stake,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeSnarkLeaf},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf>,
    // TODO: Change to EligibilityValue once PR1 is merged
    target_value: Stake,
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

    /// Get the total stake.
    // TODO: Change to EligibilityValue once PR1 is merged
    pub fn get_target_value(&self) -> Stake {
        self.target_value
    }
}
