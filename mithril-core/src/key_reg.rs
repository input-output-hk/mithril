//! Key registration functionality.

use crate::error::RegisterError;
use digest::{Digest, FixedOutput};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;

#[cfg(not(feature = "zcash"))]
use crate::multi_sig::{VerificationKey, VerificationKeyPoP};
#[cfg(feature = "zcash")]
use crate::multi_sig_zcash::{VerificationKey, VerificationKeyPoP};

use super::stm::Stake;
use crate::merkle_tree::{MTLeaf, MerkleTree};

/// Struct that collects public keys and stakes of parties. Each participant (both the
/// signers and the clerks) need to run their own instance of the key registration.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KeyReg {
    keys: HashMap<VerificationKey, Stake>,
}

/// Structure generated out of a closed registration. One can only get a global `avk` out of
/// a closed key registration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosedKeyReg<D>
where
    D: Digest + FixedOutput,
{
    /// Ordered list of registered parties
    pub reg_parties: Vec<RegParty>,
    /// Total stake of the registered parties.
    pub total_stake: Stake,
    /// Unique public key out of the key registration instance.
    pub merkle_tree: Arc<MerkleTree<D>>,
}

/// A registered party, a stake associated with its public key
pub type RegParty = MTLeaf;

impl KeyReg {
    /// Initialise an empty `KeyReg`.
    pub fn init() -> Self {
        Self {
            keys: HashMap::new(),
        }
    }

    /// Register the pubkey and stake for a particular party. These are store in a
    /// mapping `key |-> stake`
    pub fn register(&mut self, stake: Stake, pk: VerificationKeyPoP) -> Result<(), RegisterError> {
        if let Entry::Vacant(e) = self.keys.entry(pk.vk) {
            if pk.check().is_ok() {
                e.insert(stake);
                return Ok(());
            } else {
                return Err(RegisterError::InvalidKey(Box::new(pk)));
            }
        }
        Err(RegisterError::KeyRegistered(Box::new(pk.vk)))
    }

    /// End registration. Disables `KeyReg::register`. Consumes the instance of `self` and returns
    /// a `ClosedKeyReg`.
    pub fn close<D>(self) -> ClosedKeyReg<D>
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
                MTLeaf(vk, stake)
            })
            .collect::<Vec<RegParty>>();
        reg_parties.sort();

        ClosedKeyReg {
            merkle_tree: Arc::new(MerkleTree::create(&reg_parties)),
            reg_parties,
            total_stake,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(not(feature = "zcash"))]
    use crate::multi_sig::SigningKey;
    #[cfg(feature = "zcash")]
    use crate::multi_sig_zcash::SigningKey;
    use blake2::Blake2b;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    proptest! {
        #[test]
        fn test_keyreg(stake in vec(1..1u64 << 60, 2..=10),
                       nkeys in 2..10_usize,
                       fake_it in 0..4usize,
                       seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut kr = KeyReg::init();

            let gen_keys = (1..nkeys).map(|_| {
                let sk = SigningKey::gen(&mut rng);
                VerificationKeyPoP::from(&sk)
            }).collect::<Vec<_>>();

            let fake_key = {
                let sk = SigningKey::gen(&mut rng);
                VerificationKeyPoP::from(&sk)
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
                    Err(RegisterError::InvalidKey(a)) => {
                        assert_eq!(fake_it, 0);
                        assert!(a.check().is_err());
                    }
                    Err(RegisterError::SerializationError) => unreachable!(),
                }
            }

            if !kr.keys.is_empty() {
                let closed = kr.close::<Blake2b>();
                let retrieved_keys = closed.reg_parties.iter().map(|r| (r.0, r.1)).collect::<HashMap<_,_>>();
                assert!(retrieved_keys == keys);
            }
        }
    }
}
