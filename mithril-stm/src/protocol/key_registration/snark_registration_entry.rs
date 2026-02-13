use crate::membership_commitment::MerkleTreeSnarkLeaf;

use super::ClosedRegistrationEntry;

/// The type used for committing signer registrations for the Snark proof system.
pub type RegistrationEntryForSnark = MerkleTreeSnarkLeaf;

impl From<ClosedRegistrationEntry> for Option<RegistrationEntryForSnark> {
    fn from(entry: ClosedRegistrationEntry) -> Option<RegistrationEntryForSnark> {
        let vk = entry.get_verification_key_for_snark()?;
        let target = entry.get_lottery_target_value()?;
        Some(MerkleTreeSnarkLeaf(vk, target))
    }
}
