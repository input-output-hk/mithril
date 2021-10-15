//! Placeholder key registration functionality.

use std::hash::Hash;
use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;

use super::msp::{Msp, MspMvk, MspPk};
use super::{PartyId, Stake};

use ark_ec::PairingEngine;
use ark_ff::ToBytes;
use ark_std::io::Write;
use num_traits::identities::Zero;

pub struct KeyReg<PE>
where
    PE: PairingEngine,
{
    allow: bool,
    parties: HashMap<PartyId, (Stake, Option<MspPk<PE>>)>,
    keys: HashSet<MspPk<PE>>,
}

#[derive(Clone, Debug)]
pub struct RegParty<PE: PairingEngine> {
    pub party_id: PartyId,
    pub pk: MspPk<PE>,
    pub stake: Stake,
}

impl<PE: PairingEngine> RegParty<PE> {
    pub fn null_party() -> Self {
        Self {
            party_id: 0,
            pk: MspPk {
                mvk: MspMvk(PE::G2Projective::zero()),
                k1: PE::G1Projective::zero(),
                k2: PE::G1Projective::zero(),
            },
            stake: 0,
        }
    }
}

impl<PE: PairingEngine> ToBytes for RegParty<PE> {
    fn write<W: Write>(&self, mut writer: W) -> std::result::Result<(), std::io::Error> {
        self.pk.mvk.0.write(&mut writer)?;
        self.pk.k1.write(&mut writer)?;
        self.pk.k2.write(&mut writer)?;
        self.stake.write(&mut writer)
    }
}

#[derive(Debug, Clone)]
pub enum RegisterError {
    NotAllowed,
    KeyRegistered,
    PartyRegistered,
    UnknownPartyId,
    InvalidKey,
}

impl<PE> KeyReg<PE>
where
    PE: PairingEngine,
    MspPk<PE>: Hash,
{
    pub fn new(players: &[(PartyId, Stake)]) -> Self {
        let parties = players
            .iter()
            .map(|(id, stake)| { (*id, (*stake, None)) });
        Self {
            allow: true,
            parties: HashMap::from_iter(parties),
            keys: HashSet::new(),
        }
    }

    pub fn register(
        &mut self,
        party_id: PartyId,
        stake: Stake,
        pk: MspPk<PE>,
    ) -> Result<(), RegisterError> {
        if !self.allow {
            return Err(RegisterError::NotAllowed);
        }
        if self.keys.contains(&pk) {
            return Err(RegisterError::KeyRegistered);
        }

        if let Some((stake, k)) = self.parties.get_mut(&party_id) {
            if Msp::check(&pk) {
                *k = Some(pk);
                self.keys.insert(pk);
                Ok(())
            } else {
                Err(RegisterError::InvalidKey)
            }
        } else {
            Err(RegisterError::UnknownPartyId)
        }
    }

    pub fn retrieve(&self, party_id: PartyId) -> Option<RegParty<PE>> {
        let (stake, pko) = self.parties.get(&party_id)?;
        pko.map(|pk| {
            RegParty {party_id, pk: pk, stake: *stake}
        })
    }

    pub fn retrieve_all(&self) -> Vec<RegParty<PE>> {
        let mut out = vec![];
        for (party_id, (stake, pko)) in &self.parties {
            if let Some(pk) = pko {
                out.push(RegParty { party_id: *party_id, pk: *pk, stake: *stake })
            }
        }

        out
    }

    pub fn close(&mut self) {
        self.allow = false;
    }
}

impl<PE> Default for KeyReg<PE>
where
    PE: PairingEngine,
    MspPk<PE>: Hash,
{
    fn default() -> Self {
        Self::new(&[])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msp::Msp;
    use ark_bls12_377::Bls12_377;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use rand::{rngs::StdRng, SeedableRng};

    fn arb_participants(min: usize, max: usize) -> impl Strategy<Value = Vec<(PartyId, Stake)>> {
        vec(any::<Stake>(), min..=max).prop_map(|v| v.into_iter().enumerate().collect())
    }

    proptest! {
        #[test]
        fn test_keyreg(ps in arb_participants(2, 10),
                       nkeys in 2..10_usize,
                       stop in 2..10_usize,
                       seed in any::<[u8;32]>()) {
            let mut rng = StdRng::from_seed(seed);
            let mut kr = KeyReg::new(&ps);

            let gen_keys = (1..nkeys).map(|i| {
                Msp::<Bls12_377>::gen(&mut rng).1
            }).collect::<Vec<_>>();

            // Record successful registrations
            let mut keys = HashSet::new();
            let mut parties = HashSet::new();

            for (i, p) in ps.iter().enumerate() {
                let pk = gen_keys[i % gen_keys.len()];
                let reg = kr.register(p.0, p.1, pk);
                match reg {
                    Ok(_) => {
                        assert!(i <= stop);
                        assert!(keys.insert(pk));
                        assert!(parties.insert(p.0));
                    },
                    Err(RegisterError::KeyRegistered) => {
                        assert!(keys.contains(&pk));
                    }
                    Err(RegisterError::PartyRegistered) => {
                        assert!(parties.contains(&p.0));
                    }
                    Err(RegisterError::InvalidKey) => unreachable!(),
                    Err(RegisterError::UnknownPartyId) => unreachable!(),
                    Err(RegisterError::NotAllowed) => assert!(i > stop),
                }

                if i == stop {
                    kr.close();
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
