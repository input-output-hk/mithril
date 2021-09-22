//! Placeholder key registration functionality.

use crate::mithril_field::HashToCurve;
use std::collections::HashMap;

use super::msp::{Msp, MspPk};
use super::{PartyId, Stake};

use ark_ec::PairingEngine;

pub struct KeyReg<P>
where
    P: PairingEngine,
{
    allow: bool,
    store: HashMap<PartyId, (MspPk<P>, Stake)>,
}

impl<P> KeyReg<P>
where
    P: PairingEngine,
    P::G1Affine: HashToCurve,
{
    pub fn new() -> Self {
        Self {
            allow: true,
            store: HashMap::new(),
        }
    }

    pub fn register(&mut self, party_id: PartyId, stake: Stake, pk: MspPk<P>) {
        if !self.allow || self.store.contains_key(&party_id) {
            return;
        }
        if Msp::check(&pk) {
            self.store.insert(party_id, (pk, stake));
        }
    }

    pub fn retrieve(&self, party_id: PartyId) -> Option<(MspPk<P>, Stake)> {
        self.store.get(&party_id).cloned()
    }

    pub fn retrieve_all(&self) -> Vec<Option<(MspPk<P>, Stake)>> {
        let max_party_id = *self.store.keys().max().unwrap();
        (0..=max_party_id)
            .map(|p| self.store.get(&p).cloned())
            .collect()
    }

    pub fn close(&mut self) {
        self.allow = false;
    }
}
