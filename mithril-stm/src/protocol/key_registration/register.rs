use anyhow::anyhow;
use digest::{Digest, FixedOutput};
use std::collections::BTreeSet;

use crate::{
    RegisterError, SignerIndex, Stake, StmResult,
    membership_commitment::{MerkleTree, MerkleTreeLeaf},
};

use super::RegistrationEntry;

/// The type used for registering signer keys
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct KeyRegistration {
    registration_entries: BTreeSet<RegistrationEntry>,
}

impl KeyRegistration {
    /// Initialize an empty registration
    pub fn initialize() -> Self {
        Self {
            registration_entries: Default::default(),
        }
    }

    /// Check whether the given `RegistrationEntry` is already registered.
    /// Insert the new entry if all checks pass.
    /// # Error
    /// The function fails when the either of the verification keys is invalid or when the entry
    /// is already registered.
    pub fn register(&mut self, entry: &RegistrationEntry) -> StmResult<()> {
        if !self.registration_entries.contains(entry) {
            self.registration_entries.insert(*entry);
            return Ok(());
        }
        Err(anyhow!(RegisterError::EntryRegistered(Box::new(*entry))))
    }

    /// Gets the index of a signer registration entry
    pub fn get_signer_index_for_registration(
        &self,
        entry: &RegistrationEntry,
    ) -> Option<SignerIndex> {
        self.registration_entries
            .iter()
            .position(|r| r == entry)
            .map(|s| s as u64)
    }

    /// Converts the KeyRegistration into a Merkle tree
    pub fn into_merkle_tree<
        D: Digest + FixedOutput,
        L: From<RegistrationEntry> + MerkleTreeLeaf,
    >(
        self,
    ) -> StmResult<MerkleTree<D, L>> {
        Ok(MerkleTree::new(
            &self
                .registration_entries
                .iter()
                .map(|entry| (*entry).into())
                .collect::<Vec<L>>(),
        ))
    }

    /// Closes the registration by computing the total stake registered so far.
    pub fn close_registration(self) -> ClosedKeyRegistration {
        let total_stake: Stake = self.registration_entries.iter().fold(0, |acc, entry| {
            let (res, overflow) = acc.overflowing_add(entry.get_stake());
            if overflow {
                panic!("Total stake overflow");
            }
            res
        });
        ClosedKeyRegistration {
            key_registration: self.clone(),
            total_stake,
        }
    }
}

/// Closed Key Registration
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ClosedKeyRegistration {
    pub key_registration: KeyRegistration,
    pub total_stake: Stake,
}
