use crate::membership_commitment::MerkleTreeConcatenationLeaf;

use super::RegistrationEntry;

/// The type used for committing signer registrations for the Concatenation proof system.
pub type RegistrationEntryForConcatenation = MerkleTreeConcatenationLeaf;

/// Converts a registration entry into the related registration entry for concatenation proof.
impl From<RegistrationEntry> for RegistrationEntryForConcatenation {
    fn from(entry: RegistrationEntry) -> Self {
        MerkleTreeConcatenationLeaf(entry.get_bls_verification_key(), entry.get_stake())
    }
}
