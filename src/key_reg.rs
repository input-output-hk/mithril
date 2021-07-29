use std::collections::HashMap;

use super::{PartyId, Stake};
use super::msp::{self, MSP};

pub struct KeyReg {
    allow: bool,
    store: HashMap<PartyId, (msp::PK, Stake)>,
}

impl KeyReg {
    pub fn new() -> Self {
        Self {
            allow: true,
            store: HashMap::new()
        }
    }

    pub fn register(&mut self, party_id: PartyId, stake: Stake, pk: msp::PK) {
        if !self.allow || self.store.contains_key(&party_id) {
            return;
        }
        if MSP::check(&pk) {
            self.store.insert(party_id, (pk, stake));
        }
    }

    pub fn retrieve(&self, party_id: PartyId) -> Option<(msp::PK, Stake)> {
        self.store.get(&party_id).cloned()
    }

    pub fn retrieve_all(&self) -> Vec<Option<(msp::PK, Stake)>> {
        let max_party_id = *self.store.keys().max().unwrap();
        (0..max_party_id).map(|p|self.store.get(&p).cloned()).collect()
    }

    pub fn close(&mut self) {
        self.allow = false;
    }
}
