//! Key registration functionality.
use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use blake2::digest::{Digest, FixedOutput};

use crate::bls_multi_signature::{BlsVerificationKey, BlsVerificationKeyProofOfPossesion};
use crate::error::RegisterError;
use crate::merkle_tree::{MerkleTree, MerkleTreeLeaf};
use crate::Stake;

/// Stores a registered party with its public key and the associated stake.
pub type RegisteredParty = MerkleTreeLeaf;

/// Struct that collects public keys and stakes of parties.
/// Each participant (both the signers and the clerks) need to run their own instance of the key registration.
// todo: replace with KeyReg
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KeyRegistration {
    keys: HashMap<BlsVerificationKey, Stake>,
}

impl KeyRegistration {
    /// Initialize an empty `KeyReg`.
    pub fn init() -> Self {
        Self::default()
    }

    /// Verify and register a public key and stake for a particular party.
    /// # Error
    /// The function fails when the proof of possession is invalid or when the key is already registered.
    pub fn register(
        &mut self,
        stake: Stake,
        pk: BlsVerificationKeyProofOfPossesion,
    ) -> Result<(), RegisterError> {
        if let Entry::Vacant(e) = self.keys.entry(pk.vk) {
            pk.check()?;
            e.insert(stake);
            return Ok(());
        }
        Err(RegisterError::KeyRegistered(Box::new(pk.vk)))
    }

    /// Finalize the key registration.
    /// This function disables `KeyReg::register`, consumes the instance of `self`, and returns a `ClosedKeyReg`.
    pub fn close<D>(self) -> ClosedKeyRegistration<D>
    where
        D: Digest + FixedOutput,
    {
        let mut total_stake: Stake = 0;
        let mut reg_parties = self
            .keys
            .iter()
            .map(|(&vk, &stake)| {
                let (res, overflow) = total_stake.overflowing_add(stake);
                if overflow {
                    panic!("Total stake overflow");
                }
                total_stake = res;
                MerkleTreeLeaf(vk, stake)
            })
            .collect::<Vec<RegisteredParty>>();
        reg_parties.sort();

        ClosedKeyRegistration {
            merkle_tree: Arc::new(MerkleTree::create(&reg_parties)),
            reg_parties,
            total_stake,
        }
    }
}

/// Structure generated out of a closed registration containing the registered parties, total stake, and the merkle tree.
/// One can only get a global `avk` out of a closed key registration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosedKeyRegistration<D: Digest> {
    /// Ordered list of registered parties.
    pub reg_parties: Vec<RegisteredParty>,
    /// Total stake of the registered parties.
    pub total_stake: Stake,
    /// Unique public key out of the key registration instance.
    pub merkle_tree: Arc<MerkleTree<D>>,
}

#[cfg(test)]
mod tests {
    use blake2::{digest::consts::U32, Blake2b};
    use proptest::{collection::vec, prelude::*};
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::bls_multi_signature::BlsSigningKey;

    use super::*;

    proptest! {
        #[test]
        fn test_keyreg(stake in vec(1..1u64 << 60, 2..=10),
                       nkeys in 2..10_usize,
                       fake_it in 0..4usize,
                       seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut kr = KeyRegistration::init();

            let gen_keys = (1..nkeys).map(|_| {
                let sk = BlsSigningKey::generate(&mut rng);
                BlsVerificationKeyProofOfPossesion::from(&sk)
            }).collect::<Vec<_>>();

            let fake_key = {
                let sk = BlsSigningKey::generate(&mut rng);
                BlsVerificationKeyProofOfPossesion::from(&sk)
            };

            // Record successful registrations
            let mut keys = HashMap::new();

            for (i, &stake) in stake.iter().enumerate() {
                let mut pk = gen_keys[i % gen_keys.len()];

                if fake_it == 0 {
                    pk.pop = fake_key.pop;
                }

                let reg = kr.register(stake, pk);
                match reg {
                    Ok(_) => {
                        assert!(keys.insert(pk.vk, stake).is_none());
                    },
                    Err(RegisterError::KeyRegistered(pk1)) => {
                        assert!(pk1.as_ref() == &pk.vk);
                        assert!(keys.contains_key(&pk.vk));
                    }
                    Err(RegisterError::KeyInvalid(a)) => {
                        assert_eq!(fake_it, 0);
                        assert!(a.check().is_err());
                    }
                    Err(RegisterError::SerializationError) => unreachable!(),
                    _ => unreachable!(),
                }
            }

            if !kr.keys.is_empty() {
                let closed = kr.close::<Blake2b<U32>>();
                let retrieved_keys = closed.reg_parties.iter().map(|r| (r.0, r.1)).collect::<HashMap<_,_>>();
                assert!(retrieved_keys == keys);
            }
        }
    }
}
