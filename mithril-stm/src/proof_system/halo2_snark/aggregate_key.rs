use serde::{Deserialize, Serialize};

use crate::{
    MembershipDigest, Stake,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeConcatenationLeaf},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    // TODO: Change Leaf once PR1 is merged
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeConcatenationLeaf>,
    // TODO: Change to EligibilityValue once PR1 is merged
    target_value: Stake,
}
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl<D: MembershipDigest> AggregateVerificationKeyForSnark<D> {
    /// Get the Merkle tree batch commitment.
    // TODO: Change Leaf once PR1 is merged
    pub(crate) fn get_merkle_tree_commitment(
        &self,
    ) -> MerkleTreeCommitment<D::SnarkHash, MerkleTreeConcatenationLeaf> {
        self.merkle_tree_commitment.clone()
    }

    /// Get the total stake.
    // TODO: Change to EligibilityValue once PR1 is merged
    pub fn get_target_value(&self) -> Stake {
        self.target_value
    }
}
