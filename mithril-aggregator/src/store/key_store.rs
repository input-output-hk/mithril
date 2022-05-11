#![allow(dead_code)]
use super::adapters::Adapter;
use super::StoreError;
use mithril_common::crypto_helper::{ProtocolPartyId, ProtocolSignerVerificationKey};

struct SignerVerificationKeyStore {
    adapter: Box<dyn Adapter<Key = ProtocolPartyId, Record = ProtocolSignerVerificationKey>>,
}

impl SignerVerificationKeyStore {
    pub fn new(
        adapter: Box<dyn Adapter<Key = ProtocolPartyId, Record = ProtocolSignerVerificationKey>>,
    ) -> Self {
        Self { adapter }
    }

    pub fn save(
        &self,
        _party_id: ProtocolPartyId,
        _verification_key: ProtocolSignerVerificationKey,
    ) -> Result<(), StoreError> {
        todo!()
    }

    pub fn fetch(
        &self,
        party_id: &ProtocolPartyId,
    ) -> Result<Option<&ProtocolSignerVerificationKey>, StoreError> {
        self.adapter
            .get_record(party_id)
            .map_err(|e| StoreError::AdapterError(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::super::adapters::DumbAdapter;
    use super::*;
    use mithril_common::crypto_helper::tests_setup::setup_signers;

    fn generate_mithril_artefacts() -> (ProtocolPartyId, ProtocolSignerVerificationKey) {
        let (party_id, _stake, signer, _multisig) = setup_signers(1).into_iter().nth(0).unwrap();

        (party_id, signer)
    }

    #[test]
    fn test_fetch_verification_key() {
        let (party_id, signer_key) = generate_mithril_artefacts();
        let store = SignerVerificationKeyStore::new(Box::new(DumbAdapter::new(
            party_id,
            Box::new(signer_key.clone()),
        )));
        assert_eq!(&signer_key, store.fetch(&party_id).unwrap().unwrap());
    }
}
