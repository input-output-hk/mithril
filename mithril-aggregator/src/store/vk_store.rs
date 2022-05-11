use super::adapters::Adapter;
use super::StoreError;
use crate::ProtocolSignerVerificationKey;
use mithril::stm::PartyId;

struct VkStore {
    adapter: Box<dyn Adapter<Key = PartyId, Record = ProtocolSignerVerificationKey>>,
}

impl VkStore {
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
