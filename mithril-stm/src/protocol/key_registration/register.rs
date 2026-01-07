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

    pub fn get_registration_entry_for_index(
        &self,
        signer_index: &SignerIndex,
    ) -> StmResult<RegistrationEntry> {
        self.registration_entries
            .iter()
            .nth(*signer_index as usize)
            .cloned()
            .ok_or_else(|| anyhow!(RegisterError::UnregisteredIndex))
    }
}

/// Closed Key Registration
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ClosedKeyRegistration {
    pub key_registration: KeyRegistration,
    pub total_stake: Stake,
}

#[cfg(test)]
mod tests {
    use proptest::{collection::vec, prelude::*};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        VerificationKeyProofOfPossessionForConcatenation, signature_scheme::BlsSigningKey,
    };

    use super::*;

    proptest! {
        #[test]
        fn test_keyreg(stake in vec(1..1u64 << 60, 2..=10),
                       nkeys in 2..10_usize,
                       fake_it in 0..4usize,
                       seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut kr = KeyRegistration::initialize();

            let gen_keys = (1..nkeys).map(|_| {
                let sk = BlsSigningKey::generate(&mut rng);
                VerificationKeyProofOfPossessionForConcatenation::from(&sk)
            }).collect::<Vec<_>>();

            let fake_key = {
                let sk = BlsSigningKey::generate(&mut rng);
                VerificationKeyProofOfPossessionForConcatenation::from(&sk)
            };

            // Record successful registrations
            let mut keys = BTreeSet::new();

            for (i, &stake) in stake.iter().enumerate() {
                let mut pk = gen_keys[i % gen_keys.len()];

                if fake_it == 0 {
                    pk.pop = fake_key.pop;
                }

                let entry_result = RegistrationEntry::new(pk.clone(), #[cfg(feature = "future_snark")] None, stake);

                match entry_result {
                    Ok(entry) => {
                        let reg = kr.register(&entry);
                        match reg {
                            Ok(_) => {
                                assert!(keys.insert(entry));
                            },
                            Err(error) => match error.downcast_ref::<RegisterError>(){
                                Some(RegisterError::EntryRegistered(e1)) => {
                                    assert!(e1.as_ref() == &entry);
                                    assert!(keys.contains(&entry));
                                },
                                _ => {panic!("Unexpected error: {error}")}
                            }
                        }
                    },
                    Err(error) =>  match error.downcast_ref::<RegisterError>(){
                        Some(RegisterError::KeyInvalid(a)) => {
                            assert_eq!(fake_it, 0);
                            assert!(pk.verify_proof_of_possession().is_err());
                            assert!(a.as_ref() == &pk.vk);
                        },
                        _ => {panic!("Unexpected error: {error}")}
                    }
                }
            }

            if !kr.registration_entries.is_empty() {
                let closed = kr.close_registration();
                let retrieved_keys = closed.key_registration.registration_entries;
                assert!(retrieved_keys == keys);
            }
        }
    }
}
