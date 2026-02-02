use crate::membership_commitment::MerkleTreeSnarkLeaf;

use super::ClosedRegistrationEntry;

/// The type used for committing signer registrations for the Snark proof system.
pub type RegistrationEntryForSnark = MerkleTreeSnarkLeaf;

impl From<ClosedRegistrationEntry> for RegistrationEntryForSnark {
    fn from(entry: ClosedRegistrationEntry) -> Self {
        MerkleTreeSnarkLeaf(
            entry.get_verification_key_for_snark(),
            entry.get_lottery_target_value(),
        )
    }
}
