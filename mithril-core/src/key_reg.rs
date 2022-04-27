//! Placeholder key registration functionality.

use std::collections::{HashMap, HashSet};

use super::msp::{Msp, MspPk};
use super::stm::{PartyId, Stake};

use crate::merkle_tree::{MTHashLeaf, MerkleTree};
use crate::stm::MTValue;

/// Simple struct that collects pubkeys and stakes of parties. Placeholder for a more
/// realistic distributed key registration protocol.
#[derive(Clone, Debug)]
pub struct KeyReg
{
    parties: HashMap<PartyId, Party>,
    // `keys` is just the set of all of the keys that have been registered
    // (i.e., in `parties`)
    keys: HashSet<MspPk>,
}

/// Structure generated out of a closed registration. One can only get a global `avk` out of
/// a closed key registration.
#[derive(Clone, Debug)]
pub struct ClosedKeyReg<H>
where
    H: MTHashLeaf,
{
    key_reg: KeyReg,
    /// Total stake of the registered parties.
    pub total_stake: Stake,
    /// Unique public key out of the key registration instance.
    pub avk: MerkleTree<H>,
}

/// Represents the status of a known participant in the protocol who is allowed
/// to register their key. `RegParty` values will be produced from mappings
/// (id |-> Party { stake, Some(key) }) in key_reg.parties
#[derive(Clone, Debug)]
struct Party
{
    /// The stake of of the party.
    pub stake: Stake,
    /// The public key of the party.
    pub pk: Option<MspPk>,
}

#[derive(Clone, Debug, PartialEq)]
/// A registered party, i.e. an id associated with its stake and public key
pub struct RegParty
{
    /// The id for the registered party.
    pub party_id: PartyId,
    /// The pubkey for the registered party.
    pub pk: MspPk,
    /// The stake for the registered party.
    pub stake: Stake,
}

// impl<PE: PairingEngine> ToBytes for RegParty {
//     fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
//         self.pk.mvk.0.write(&mut writer)?;
//         self.pk.k1.write(&mut writer)?;
//         self.pk.k2.write(&mut writer)?;
//         self.stake.write(&mut writer)
//     }
// }

/// Errors which can be outputted by key registration.
#[derive(Debug, Clone, thiserror::Error)]
pub enum RegisterError
{
    /// This key has already been registered by a participant
    #[error("This key has already been registered.")]
    KeyRegistered([u8; 48]),
    /// This participant has already been registered
    #[error("Participant {0} has already been registered.")]
    PartyRegistered(PartyId),
    /// The supplied participant id does not belong to the
    /// participant list
    #[error("Participant id {0} does not belong to the participants list.")]
    UnknownPartyId(PartyId),
    /// The supplied key is not valid
    #[error("The verification of correctness of the supplied key is invalid.")]
    InvalidKey(MspPk),
}

impl KeyReg
{
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
    pub fn register(&mut self, party_id: PartyId, pk: MspPk) -> Result<(), RegisterError> {
        if self.keys.contains(&pk) {
            return Err(RegisterError::KeyRegistered(pk.mvk.to_bytes()));
        }

        if let Some(mut party) = self.parties.get_mut(&party_id) {
            if Msp::check(&pk) {
                party.pk = Some(pk);
                self.keys.insert(pk);
                Ok(())
            } else {
                Err(RegisterError::InvalidKey(pk))
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

    /// Retrieve the pubkey and stake for all parties.
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
    pub fn close<H>(self) -> ClosedKeyReg<H>
    where
        H: MTHashLeaf,
    {
        let mtvals: Vec<MTValue> = self
            .retrieve_all()
            .iter()
            .map(|rp| MTValue(rp.pk.mvk, rp.stake))
            .collect();

        ClosedKeyReg {
            key_reg: self,
            total_stake: mtvals.iter().map(|s| s.1).sum(),
            avk: MerkleTree::create(&mtvals.iter().map(|&s| s.to_bytes().to_vec()).collect::<Vec<Vec<u8>>>()),
        }
    }
}

impl Default for KeyReg
{
    fn default() -> Self {
        Self::new(&[])
    }
}

impl<H> ClosedKeyReg<H>
where
    H: MTHashLeaf,
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
    use blst::blst_p1_generator;
    use super::*;
    use crate::msp::Msp;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    fn arb_participants(min: usize, max: usize) -> impl Strategy<Value = Vec<(PartyId, Stake)>> {
        vec(any::<Stake>(), min..=max).prop_map(|v| v.into_iter().enumerate().collect())
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
                Msp::gen(&mut rng).1
            }).collect::<Vec<_>>();

            // Record successful registrations
            let mut keys = HashSet::new();
            let mut parties = HashSet::new();

            for (i, p) in ps.iter().enumerate() {
                let mut pk = gen_keys[i % gen_keys.len()];

                if fake_it == 0 {
                   pk.pop.k2 = unsafe { *blst_p1_generator() };
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
                        assert!(pk1 == pk.mvk.to_bytes());
                        assert!(keys.contains(&pk));
                    }
                    Err(RegisterError::PartyRegistered(party)) => {
                        assert!(party == p.0);
                        assert!(parties.contains(&party));
                    }
                    Err(RegisterError::InvalidKey(a)) => {
                        assert_eq!(fake_it, 0);
                        assert!(!Msp::check(&a));
                    }
                    Err(RegisterError::UnknownPartyId(_)) => assert_eq!(fake_it, 1),
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
