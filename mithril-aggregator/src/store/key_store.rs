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

    pub fn save_vk(
        &self,
        party_id: PartyId,
        verification_key: ProtocolSignerVerificationKey,
    ) -> Result<(), StoreError> {
        unimplemented!()
    }
}
/*
#[tests]
mod tests {
    #[test]
    fn
}
*/
