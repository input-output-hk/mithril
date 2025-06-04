use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::merkle_tree::{BatchPath, MerkleTreeCommitmentBatchCompat};
use crate::{ClosedKeyReg, Stake};

/// Stm aggregate key (batch compatible), which contains the merkle tree commitment and the total stake of the system.
/// Batch Compat Merkle tree commitment includes the number of leaves in the tree in order to obtain batch path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "BatchPath<D>: Serialize",
    deserialize = "BatchPath<D>: Deserialize<'de>"
))]
pub struct StmAggrVerificationKey<D: Clone + Digest + FixedOutput> {
    mt_commitment: MerkleTreeCommitmentBatchCompat<D>,
    total_stake: Stake,
}

impl<D: Digest + Clone + FixedOutput> StmAggrVerificationKey<D> {
    pub fn get_mt_commitment(&self) -> MerkleTreeCommitmentBatchCompat<D> {
        self.mt_commitment.clone()
    }

    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }
}

impl<D: Digest + Clone + FixedOutput> PartialEq for StmAggrVerificationKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: Digest + Clone + FixedOutput> Eq for StmAggrVerificationKey<D> {}

impl<D: Clone + Digest + FixedOutput> From<&ClosedKeyReg<D>> for StmAggrVerificationKey<D> {
    fn from(reg: &ClosedKeyReg<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_commitment_batch_compat(),
            total_stake: reg.total_stake,
        }
    }
}
