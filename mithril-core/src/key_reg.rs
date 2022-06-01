//! Key registration functionality.

use crate::error::RegisterError;
use digest::{Digest, FixedOutput};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::multi_sig::VerificationKeyPoP;
use super::stm::{PartyId, Stake};

use crate::merkle_tree::{MTLeaf, MerkleTree};
use crate::multi_sig::VerificationKey;

/// Struct that collects public keys and stakes of parties. Each participant (both the
/// signers and the clerks) need to run their own instance of the key registration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeyReg {
    parties: HashMap<PartyId, Party>,
    // `keys` is just the set of all of the keys that have been registered
    // which allows us for a check check on whether the key already exists.
    keys: HashSet<VerificationKey>,
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

/// Represents the status of a known participant in the protocol who is allowed
/// to register their key. `RegParty` values will be produced from mappings
/// (id |-> Party { stake, Some(key) }) in key_reg.parties
#[derive(Clone, Debug, PartialEq, Eq)]
struct Party {
    /// The stake of of the party.
    stake: Stake,
    /// The public key of the party.
    vk: Option<VerificationKey>,
}

/// A registered party, a stake associated with its public key
pub type RegParty = MTLeaf;

impl KeyReg {
    /// Initialise a KeyReg with all eligible parties and stakes known.
    pub fn init(players: &[(PartyId, Stake)]) -> Self {
        let parties = players.iter().map(|(id, stake)| {
            let party = Party {
                stake: *stake,
                vk: None,
            };
            (*id, party)
        });
        Self {
            parties: parties.collect(),
            keys: HashSet::new(),
        }
    }

    /// Register the pubkey for a particular party.
    pub fn register(
        &mut self,
        party_id: PartyId,
        pk: VerificationKeyPoP,
    ) -> Result<(), RegisterError> {
        if self.keys.contains(&pk.vk) {
            return Err(RegisterError::KeyRegistered(pk.vk));
        }

        if let Some(mut party) = self.parties.get_mut(&party_id) {
            if pk.check().is_ok() {
                party.vk = Some(pk.vk);
                self.keys.insert(pk.vk);
                Ok(())
            } else {
                Err(RegisterError::InvalidKey(Box::new(pk)))
            }
        } else {
            Err(RegisterError::UnknownPartyId(party_id))
        }
    }

    /// End registration. Disables `KeyReg::register`. Consumes the instance of `self` and returns
    /// a `ClosedKeyReg`.
    pub fn close<D>(self) -> ClosedKeyReg<D>
    where
        D: Digest + FixedOutput,
    {
        let mut total_stake: u64 = 0;
        let mut reg_parties = self
            .parties
            .iter()
            .filter_map(|(_, party)| {
                if let Some(vk) = party.vk {
                    total_stake = (total_stake)
                        .checked_add(party.stake)
                        .expect("stake overflow!");
                    return Some(MTLeaf(vk, party.stake));
                }
                None
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

impl Default for KeyReg {
    fn default() -> Self {
        Self::init(&[])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::multi_sig::SigningKey;
    use blake2::Blake2b;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    fn arb_participants(min: usize, max: usize) -> impl Strategy<Value = Vec<(PartyId, Stake)>> {
        vec(any::<Stake>(), min..=max).prop_map(|v| {
            v.into_iter()
                .enumerate()
                .map(|(index, value)| (index as u64, value))
                .collect()
        })
    }

    proptest! {
        #[test]
        fn test_keyreg(ps in arb_participants(2, 10),
                       nkeys in 2..10_usize,
                       fake_it in 0..4usize,
                       seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let mut kr = KeyReg::init(&ps);

            let gen_keys = (1..nkeys).map(|_| {
                let sk = SigningKey::gen(&mut rng);
                VerificationKeyPoP::from(&sk)
            }).collect::<Vec<_>>();

            let fake_key = {
                let sk = SigningKey::gen(&mut rng);
                VerificationKeyPoP::from(&sk)
            };

            // Record successful registrations
            let mut keys = HashSet::new();
            let mut parties = HashSet::new();

            for (i, p) in ps.iter().enumerate() {
                let mut pk = gen_keys[i % gen_keys.len()];

                if fake_it == 0 {
                   pk.pop = fake_key.pop;
                }

                let mut id = p.0;
                if fake_it == 1 {
                    id = 9999;
                }

                let reg = kr.register(id, pk);
                match reg {
                    Ok(_) => {
                        assert!(keys.insert(pk.vk));
                        assert!(parties.insert(p.0));
                    },
                    Err(RegisterError::KeyRegistered(pk1)) => {
                        assert!(pk1 == pk.vk);
                        assert!(keys.contains(&pk.vk));
                    }
                    Err(RegisterError::PartyRegistered(party)) => {
                        assert!(party == p.0);
                        assert!(parties.contains(&party));
                    }
                    Err(RegisterError::InvalidKey(a)) => {
                        assert_eq!(fake_it, 0);
                        assert!(a.check().is_err());
                    }
                    Err(RegisterError::UnknownPartyId(_)) => assert_eq!(fake_it, 1),
                    Err(RegisterError::SerializationError) => unreachable!(),
                }
            }

            let retrieved_ids = kr.parties.iter().filter_map(|(key, value)| {
                if value.vk.is_some() {
                    return Some(*key);
                }
                None
            } ).collect::<HashSet<_>>();
            if !retrieved_ids.is_empty() {
                let closed = kr.close::<Blake2b>();
                let retrieved_keys = closed.reg_parties.iter().map(|r| r.0).collect::<HashSet<_>>();
                assert!(retrieved_keys == keys);
            }

            assert!(retrieved_ids == parties);

        }
    }
}
