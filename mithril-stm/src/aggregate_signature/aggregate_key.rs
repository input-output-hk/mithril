use blake2::digest::{Digest, FixedOutput};
use serde::{Deserialize, Serialize};

use crate::merkle_tree::{MerkleBatchPath, MerkleTreeBatchCommitment};
use crate::{ClosedKeyRegistration, Stake};

/// Stm aggregate key (batch compatible), which contains the merkle tree commitment and the total stake of the system.
/// Batch Compat Merkle tree commitment includes the number of leaves in the tree in order to obtain batch path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "MerkleBatchPath<D>: Serialize",
    deserialize = "MerkleBatchPath<D>: Deserialize<'de>"
))]
pub struct AggregateVerificationKey<D: Clone + Digest + FixedOutput> {
    mt_commitment: MerkleTreeBatchCommitment<D>,
    total_stake: Stake,
}

impl<D: Digest + Clone + FixedOutput> AggregateVerificationKey<D> {
    pub fn get_mt_commitment(&self) -> MerkleTreeBatchCommitment<D> {
        self.mt_commitment.clone()
    }

    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }
}

impl<D: Digest + Clone + FixedOutput> PartialEq for AggregateVerificationKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.mt_commitment == other.mt_commitment && self.total_stake == other.total_stake
    }
}

impl<D: Digest + Clone + FixedOutput> Eq for AggregateVerificationKey<D> {}

impl<D: Clone + Digest + FixedOutput> From<&ClosedKeyRegistration<D>>
    for AggregateVerificationKey<D>
{
    fn from(reg: &ClosedKeyRegistration<D>) -> Self {
        Self {
            mt_commitment: reg.merkle_tree.to_commitment_batch_compat(),
            total_stake: reg.total_stake,
        }
    }
}
