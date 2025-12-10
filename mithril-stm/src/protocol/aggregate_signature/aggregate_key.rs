use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, Stake,
    membership_commitment::{MembershipDigest, MerkleBatchPath, MerkleTreeBatchCommitment},
};

/// Stm aggregate key (batch compatible), which contains the merkle tree commitment and the total stake of the system.
/// Batch Compat Merkle tree commitment includes the number of leaves in the tree in order to obtain batch path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub struct AggregateVerificationKey<D: MembershipDigest> {
    mt_commitment: MerkleTreeBatchCommitment<D::ConcatenationHash>,
    total_stake: Stake,
}

impl<D: MembershipDigest> AggregateVerificationKey<D> {
    pub(crate) fn get_merkle_tree_batch_commitment(
        &self,
    ) -> MerkleTreeBatchCommitment<D::ConcatenationHash> {
        self.mt_commitment.clone()
    }

    #[deprecated(
        since = "0.5.0",
        note = "Use `get_merkle_tree_batch_commitment` instead"
    )]
    pub fn get_mt_commitment(&self) -> MerkleTreeBatchCommitment<D::ConcatenationHash> {
        Self::get_merkle_tree_batch_commitment(self)
    }

    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKey<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration<D>> for AggregateVerificationKey<D> {
    fn from(reg: &ClosedKeyRegistration<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_merkle_tree_batch_commitment(),
            total_stake: reg.total_stake,
        }
    }
}
