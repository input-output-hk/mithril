#![allow(dead_code)]
use super::adapters::Adapter;
use super::StoreError;
use crate::ProtocolSignerVerificationKey;
use mithril::stm::PartyId;

struct SignerVerificationKeyStore {
    adapter: Box<dyn Adapter<Key = PartyId, Record = ProtocolSignerVerificationKey>>,
}

impl SignerVerificationKeyStore {
    pub fn new(
        adapter: Box<dyn Adapter<Key = PartyId, Record = ProtocolSignerVerificationKey>>,
    ) -> Self {
        Self { adapter }
    }

    pub fn save(
        &self,
        _party_id: PartyId,
        _verification_key: ProtocolSignerVerificationKey,
    ) -> Result<(), StoreError> {
        unimplemented!()
    }

    pub fn fetch(
        &self,
        _party_id: PartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, StoreError> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    /*
       use super::super::adapters::DumbAdapter;
       #[test]
       fn create_dumb_adapter() -> DumbAdapter {
           DumbAdapter::new(44 as PartyId, Box::new())
       }
    */
}
