use serde::{Deserialize, Serialize};

use crate::{
    ClosedKeyRegistration, MembershipDigest, RegistrationEntryForSnark, Stake, StmResult,
    membership_commitment::{MerkleTreeCommitment, MerkleTreeError, MerkleTreeSnarkLeaf},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateVerificationKeyForSnark<D: MembershipDigest> {
    merkle_tree_commitment: MerkleTreeCommitment<D::SnarkHash, MerkleTreeSnarkLeaf>,
    total_stake: Stake,
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
    pub fn get_total_stake(&self) -> Stake {
        self.total_stake
    }

    /// Get the target value.
    // TODO: Change to EligibilityValue once PR1 is merged
    pub fn get_target_value(&self) -> Stake {
        self.target_value
    }

    /// Convert the aggregate verification key for concatenation to bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(self.merkle_tree_commitment.to_bytes()); // TODO: support dynamic length for the commitment
        bytes.extend(self.total_stake.to_be_bytes());
        bytes.extend(self.target_value.to_be_bytes());

        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut u64_bytes = [0u8; 8];
        let size = bytes.len();

        u64_bytes.copy_from_slice(&bytes[size - 8..]);
        let stake = u64::from_be_bytes(u64_bytes);
        let mt_commitment = MerkleTreeCommitment::from_bytes(
            bytes.get(..size - 8).ok_or(MerkleTreeError::SerializationError)?,
        )?;

        Ok(Self {
            merkle_tree_commitment: mt_commitment,
            total_stake: stake,
            target_value: stake,
        })
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKeyForSnark<D> {
    fn eq(&self, other: &Self) -> bool {
        /* self.merkle_tree_commitment == other.merkle_tree_commitment
        && self.target_value == other.target_value */
        // TODO: fix
        todo!()
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKeyForSnark<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration> for AggregateVerificationKeyForSnark<D> {
    fn from(registration: &ClosedKeyRegistration) -> Self {
        Self {
            merkle_tree_commitment: registration
                .to_merkle_tree::<D::SnarkHash, RegistrationEntryForSnark>()
                .to_merkle_tree_commitment(),
            total_stake: registration.total_stake,
            target_value: registration.total_stake,
        }
    }
}
