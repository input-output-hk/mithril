use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeSnarkLeaf},
    protocol::RegistrationEntryForSnark,
};

/// Aggregate verification key of the snark proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf>,
}

impl<D: MembershipDigest> AggregateVerificationKeyForSnark<D> {
    /// Get the Merkle tree commitment.
    pub(crate) fn get_merkle_tree_commitment(
        &self,
    ) -> &MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf> {
        &self.merkle_tree_commitment
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKeyForSnark<D> {
    fn eq(&self, other: &Self) -> bool {
        self.merkle_tree_commitment == other.merkle_tree_commitment
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKeyForSnark<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration> for AggregateVerificationKeyForSnark<D> {
    fn from(reg: &ClosedKeyRegistration) -> Self {
        let key_registration_commitment_for_snark =
            reg.to_merkle_tree::<D::SnarkHash, RegistrationEntryForSnark>();
        Self {
            merkle_tree_commitment: key_registration_commitment_for_snark
                .to_merkle_tree_commitment(),
        }
    }
}
