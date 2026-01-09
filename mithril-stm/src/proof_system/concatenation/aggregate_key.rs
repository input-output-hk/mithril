use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest, RegistrationEntryForConcatenation, Stake,
    membership_commitment::{
        MerkleBatchPath, MerkleTreeBatchCommitment, MerkleTreeConcatenationLeaf,
    },
};

/// Aggregate verification key of the concatenation proof system.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub struct AggregateVerificationKeyForConcatenation<D: MembershipDigest> {
    mt_commitment: MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf>,
    total_stake: Stake,
}

impl<D: MembershipDigest> AggregateVerificationKeyForConcatenation<D> {
    /// Get the Merkle tree batch commitment.
    pub(crate) fn get_merkle_tree_batch_commitment(
        &self,
    ) -> MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf> {
        self.mt_commitment.clone()
    }

    /// Get the total stake.
    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKeyForConcatenation<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKeyForConcatenation<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration>
    for AggregateVerificationKeyForConcatenation<D>
{
    fn from(reg: &ClosedKeyRegistration) -> Self {
        Self {
            mt_commitment: reg
                .key_registration
                .clone()
                .into_merkle_tree::<D::ConcatenationHash, RegistrationEntryForConcatenation>()
                .unwrap()
                .to_merkle_tree_batch_commitment(),
            total_stake: reg.total_stake,
        }
    }
}
