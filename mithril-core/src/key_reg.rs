//! Placeholder key registration functionality.

use crate::error::RegisterError;
use digest::{Digest, FixedOutput};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::msp::VerificationKeyPoP;
use super::stm::{PartyId, Stake};

use crate::merkle_tree::MerkleTree;
use crate::stm::MTValue;

/// Simple struct that collects pubkeys and stakes of parties. Placeholder for a more
/// realistic distributed key registration protocol.
#[derive(Clone, Debug)]
pub struct KeyReg {
    parties: HashMap<PartyId, Party>,
    // `keys` is just the set of all of the keys that have been registered
    // (i.e., in `parties`)
    keys: HashSet<VerificationKeyPoP>,
}

/// Structure generated out of a closed registration. One can only get a global `avk` out of
/// a closed key registration.
#[derive(Clone, Debug)]
pub struct ClosedKeyReg<D>
where
    D: Digest + FixedOutput,
{
    key_reg: KeyReg,
    /// Total stake of the registered parties.
    pub total_stake: Stake,
    /// Unique public key out of the key registration instance.
    pub avk: Arc<MerkleTree<D>>,
}

/// Represents the status of a known participant in the protocol who is allowed
/// to register their key. `RegParty` values will be produced from mappings
/// (id |-> Party { stake, Some(key) }) in key_reg.parties
#[derive(Clone, Debug)]
struct Party {
    /// The stake of of the party.
    pub stake: Stake,
    /// The public key of the party.
    pub pk: Option<VerificationKeyPoP>,
}

#[derive(Clone, Debug, PartialEq)]
/// A registered party, i.e. an id associated with its stake and public key
pub struct RegParty {
    /// The id for the registered party.
    // todo: I don't see the goal of the identifier
    pub party_id: PartyId,
    /// The pubkey for the registered party.
    pub pk: VerificationKeyPoP,
    /// The stake for the registered party.
    pub stake: Stake,
}

impl RegParty {
    /// Convert to bytes
    /// # Layout
    /// The layout of a `RegParty` encoding is
    /// * Party index (position in the merkle tree)
    /// * Public key
    /// * Stake (as an 8 byte tuple)
    pub fn to_bytes(&self) -> [u8; 208] {
        let mut output = [0u8; 208];
        output[..8].copy_from_slice(&self.party_id.to_be_bytes());
        output[8..200].copy_from_slice(&self.pk.to_bytes());
        output[200..].copy_from_slice(&self.stake.to_be_bytes());
        output
    }

    /// Convert an array of bytes to a `RegParty`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, RegisterError> {
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[..8]);
        let party_id = u64::from_be_bytes(u64_bytes);

        let pk = VerificationKeyPoP::from_bytes(&bytes[8..])?;
        u64_bytes.copy_from_slice(&bytes[200..]);
        let stake = u64::from_be_bytes(u64_bytes);

        Ok(Self {
            party_id,
            pk,
            stake,
        })
    }
}

impl KeyReg {
    /// Create a new KeyReg with all parties and stakes known.
    pub fn new(players: &[(PartyId, Stake)]) -> Self {
        let parties = players.iter().map(|(id, stake)| {
            let party = Party {
                stake: *stake,
                pk: None,
            };
            (*id, party)
        });
        Self {
            parties: parties.collect(),
            keys: HashSet::new(),
        }
    }

    /// Register the pubkey for a particular party.
    pub fn register(&mut self, party_id: PartyId, pk: VerificationKeyPoP) -> Result<(), RegisterError> {
        if self.keys.contains(&pk) {
            return Err(RegisterError::KeyRegistered(pk.vk.to_bytes()));
        }

        if let Some(mut party) = self.parties.get_mut(&party_id) {
            if pk.check().is_ok() {
                party.pk = Some(pk);
                self.keys.insert(pk);
                Ok(())
            } else {
                Err(RegisterError::InvalidKey(Box::new(pk)))
            }
        } else {
            Err(RegisterError::UnknownPartyId(party_id))
        }
    }

    /// Retrieve the pubkey and stake for a party.
    pub fn retrieve(&self, party_id: PartyId) -> Option<RegParty> {
        let party = self.parties.get(&party_id)?;
        party.pk.map(|pk| RegParty {
            party_id,
            pk,
            stake: party.stake,
        })
    }

    /// Retrieve the pubkey and stake for all parties. The keys are ordered,
    /// to provide a consistent merkle commitment.
    pub fn retrieve_all(&self) -> Vec<RegParty> {
        let mut out = vec![];
        let mut ps = self.parties.keys().collect::<Vec<_>>();
        ps.sort();
        for party_id in ps {
            let party = &self.parties[party_id];
            if let Some(pk) = party.pk {
                out.push(RegParty {
                    party_id: *party_id,
                    pk,
                    stake: party.stake,
                })
            }
        }

        out
    }

    /// End registration. Disables `KeyReg::register`. Consumes the instance of `self` and returns
    /// a `ClosedKeyReg`.
    pub fn close<D>(self) -> ClosedKeyReg<D>
    where
        D: Digest + FixedOutput,
    {
        let mtvals: Vec<MTValue> = self
            .retrieve_all()
            .iter()
            .map(|rp| MTValue(rp.pk.vk, rp.stake))
            .collect();

        ClosedKeyReg {
            key_reg: self,
            total_stake: mtvals.iter().map(|s| s.1).sum(),
            avk: Arc::new(MerkleTree::create(
                &mtvals
                    .iter()
                    .map(|&s| s.to_bytes().to_vec())
                    .collect::<Vec<Vec<u8>>>(),
            )),
        }
    }
}

impl Default for KeyReg {
    fn default() -> Self {
        Self::new(&[])
    }
}

impl<D> ClosedKeyReg<D>
where
    D: Digest + FixedOutput,
{
    /// Retrieve the pubkey and stake for a party.
    pub fn retrieve(&self, party_id: PartyId) -> Option<RegParty> {
        self.key_reg.retrieve(party_id)
    }

    /// Retrieve the pubkey and stake for all parties.
    pub fn retrieve_all(&self) -> Vec<RegParty> {
        self.key_reg.retrieve_all()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msp::SigningKey;
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
            let mut kr = KeyReg::new(&ps);

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
                        assert!(keys.insert(pk));
                        assert!(parties.insert(p.0));
                    },
                    Err(RegisterError::KeyRegistered(pk1)) => {
                        assert!(pk1 == pk.vk.to_bytes());
                        assert!(keys.contains(&pk));
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

            let registered = kr.retrieve_all();
            let retrieved_keys = registered.iter().map(|r| r.pk).collect::<HashSet<_>>();
            let retrieved_ids = registered.iter().map(|r| r.party_id).collect::<HashSet<_>>();

            assert!(retrieved_ids == parties);
            assert!(retrieved_keys == keys);

        }
    }
}
