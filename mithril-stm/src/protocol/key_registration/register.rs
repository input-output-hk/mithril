use digest::{Digest, FixedOutput};
use std::collections::BTreeSet;

use crate::{
    RegisterError, SignerIndex, Stake, StmResult, VerificationKeyProofOfPossessionForConcatenation,
    membership_commitment::{MerkleTree, MerkleTreeLeaf},
};

use super::RegistrationEntry;

/// Key Registration
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
    /// The function fails when the entry is already registered.
    pub fn register_by_entry(&mut self, entry: &RegistrationEntry) -> StmResult<()> {
        if !self.registration_entries.contains(entry) {
            self.registration_entries.insert(*entry);
            return Ok(());
        }
        Err(RegisterError::KeyRegistered(Box::new(entry.get_bls_verification_key())).into())
    }

    /// Registers a new signer with the given verification key proof of possession and stake.
    /// This function only works for concatenation proof system.
    /// The purpose of this function is to simplify the process for the rest of the codebase.
    pub fn register(
        &mut self,
        stake: Stake,
        vk_pop: &VerificationKeyProofOfPossessionForConcatenation,
    ) -> StmResult<()> {
        let entry = RegistrationEntry::new(*vk_pop, stake)?;
        self.register_by_entry(&entry)
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

    /// Get the registration entry for a given signer index.
    pub fn get_registration_entry_for_index(
        &self,
        signer_index: &SignerIndex,
    ) -> StmResult<RegistrationEntry> {
        self.registration_entries
            .iter()
            .nth(*signer_index as usize)
            .cloned()
            .ok_or_else(|| RegisterError::UnregisteredIndex.into())
    }

    /// Converts the KeyRegistration into a Merkle tree
    pub fn into_merkle_tree<
        D: Digest + FixedOutput,
        L: From<RegistrationEntry> + MerkleTreeLeaf,
    >(
        &self,
    ) -> MerkleTree<D, L> {
        MerkleTree::new(
            &self
                .registration_entries
                .iter()
                .map(|entry| (*entry).into())
                .collect::<Vec<L>>(),
        )
    }

    /// Closes the registration by computing the total stake registered.
    pub fn close_registration(self) -> ClosedKeyRegistration {
        let total_stake: Stake = self.registration_entries.iter().fold(0, |acc, entry| {
            let (res, overflow) = acc.overflowing_add(entry.get_stake());
            if overflow {
                panic!(
                    "Total stake overflow accumulated stake: {}, adding stake: {}",
                    acc,
                    entry.get_stake()
                );
            }
            res
        });
        ClosedKeyRegistration {
            key_registration: self,
            total_stake,
        }
    }
}

/// Closed Key Registration
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct ClosedKeyRegistration {
    /// The key registration entries
    pub key_registration: KeyRegistration,

    /// The total stake registered
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

                let entry_result = RegistrationEntry::new(pk, stake);

                match entry_result {
                    Ok(entry) => {
                        let reg = kr.register_by_entry(&entry);
                        match reg {
                            Ok(_) => {
                                assert!(keys.insert(entry));
                            },
                            Err(error) => match error.downcast_ref::<RegisterError>(){
                                Some(RegisterError::KeyRegistered(e1)) => {
                                    assert!(e1.as_ref() == &entry.get_bls_verification_key());
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

    mod golden {
        use blake2::{Blake2b, digest::consts::U32};

        use crate::{
            Initializer, Parameters,
            membership_commitment::{MerkleTreeBatchCommitment, MerkleTreeConcatenationLeaf},
        };

        use super::*;

        const GOLDEN_JSON: &str = r#"
        {
            "root":[4,3,108,183,145,65,166,69,250,202,51,64,90,232,45,103,56,138,102,63,209,245,81,22,120,16,6,96,140,204,210,55],
            "nr_leaves":4,
            "hasher":null
        }"#;

        fn golden_value() -> MerkleTreeBatchCommitment<Blake2b<U32>, MerkleTreeConcatenationLeaf> {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let params = Parameters {
                m: 10,
                k: 5,
                phi_f: 0.8,
            };
            let number_of_parties = 4;

            let mut key_reg = KeyRegistration::initialize();
            for stake in 0..number_of_parties {
                let initializer = Initializer::new(params, stake, &mut rng);
                key_reg.register_by_entry(&initializer.clone().into()).unwrap();
            }

            let closed_key_reg: ClosedKeyRegistration = key_reg.close_registration();
            closed_key_reg
                .key_registration
                .into_merkle_tree()
                .to_merkle_tree_batch_commitment()
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
