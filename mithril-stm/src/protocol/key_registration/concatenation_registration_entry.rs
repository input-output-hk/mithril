use crate::membership_commitment::MerkleTreeConcatenationLeaf;

use super::ClosedRegistrationEntry;

/// The type used for committing signer registrations for the Concatenation proof system.
pub type RegistrationEntryForConcatenation = MerkleTreeConcatenationLeaf;

impl From<ClosedRegistrationEntry> for RegistrationEntryForConcatenation {
    fn from(entry: ClosedRegistrationEntry) -> Self {
        MerkleTreeConcatenationLeaf(
            entry.get_verification_key_for_concatenation(),
            entry.get_stake(),
        )
    }
}
