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

    pub fn register(&mut self, party_id: PartyId, stake: Stake, pk: MspPk<PE>) {
        if !self.allow || self.keys.contains(&pk) {
            return;
        }

        if let Some((stake, k)) = self.parties.get_mut(&party_id) {
            if Msp::check(&pk) {
                *k = Some(pk);
                self.keys.insert(pk);
            }
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
