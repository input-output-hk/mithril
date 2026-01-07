use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest, RegistrationEntryForConcatenation, Stake,
    membership_commitment::{
        MerkleBatchPath, MerkleTreeBatchCommitment, MerkleTreeConcatenationLeaf,
    },
};

/// Stm aggregate key (batch compatible), which contains the merkle tree commitment and the total stake of the system.
/// Batch Compat Merkle tree commitment includes the number of leaves in the tree in order to obtain batch path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D::ConcatenationHash>: Serialize",
    deserialize = "MerkleBatchPath<D::ConcatenationHash>: Deserialize<'de>"
))]
pub struct ConcatenationProofKey<D: MembershipDigest> {
    mt_commitment: MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf>,
    total_stake: Stake,
}

impl<D: MembershipDigest> ConcatenationProofKey<D> {
    pub(crate) fn get_merkle_tree_batch_commitment(
        &self,
    ) -> MerkleTreeBatchCommitment<D::ConcatenationHash, MerkleTreeConcatenationLeaf> {
        self.mt_commitment.clone()
    }

    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }
}

impl<D: MembershipDigest> PartialEq for ConcatenationProofKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: MembershipDigest> Eq for ConcatenationProofKey<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration> for ConcatenationProofKey<D> {
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
