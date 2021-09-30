//! Placeholder key registration functionality.

use crate::msp::MspMvk;
use std::collections::HashMap;

use super::msp::{Msp, MspPk};
use super::{PartyId, Stake};

use num_traits::identities::Zero;
use ark_ec::{PairingEngine};
use ark_ff::ToBytes;
use ark_std::io::Write;

pub struct KeyReg<PE>
where
    PE: PairingEngine,
{
    allow: bool,
    store: HashMap<PartyId, RegParty<PE>>,
}

#[derive(Clone, Debug)]
pub struct RegParty<PE: PairingEngine> {
    pub pk: MspPk<PE>,
    pub stake: Stake,
}

impl<PE: PairingEngine> RegParty<PE> {
    pub fn null_party() -> Self {
        Self {
            pk: MspPk {
                mvk: MspMvk(PE::G2Projective::zero()),
                k1: PE::G1Projective::zero(),
                k2: PE::G1Projective::zero(),
            },
            stake: 0
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
{
    pub fn new() -> Self {
        Self {
            allow: true,
            store: HashMap::new(),
        }
    }

    pub fn register(&mut self, party_id: PartyId, stake: Stake, pk: MspPk<PE>) {
        if !self.allow || self.store.contains_key(&party_id) {
            return;
        }
        if Msp::check(&pk) {
            self.store.insert(
                party_id,
                RegParty {
                    pk: pk,
                    stake: stake,
                },
            );
        }
    }

    pub fn retrieve(&self, party_id: PartyId) -> Option<RegParty<PE>> {
        self.store.get(&party_id).cloned()
    }

    pub fn retrieve_all(&self) -> Vec<Option<RegParty<PE>>> {
        let max_party_id = *self.store.keys().max().unwrap();
        (0..=max_party_id).map(|p| self.retrieve(p)).collect()
    }

    pub fn close(&mut self) {
        self.allow = false;
    }
}
