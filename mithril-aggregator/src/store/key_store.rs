#![allow(dead_code)]
use super::adapters::Adapter;
use super::StoreError;
use mithril_common::crypto_helper::{ProtocolPartyId, ProtocolSignerVerificationKey};
use std::sync::RwLock;

struct SignerVerificationKeyStore {
    adapter:
        RwLock<Box<dyn Adapter<Key = ProtocolPartyId, Record = ProtocolSignerVerificationKey>>>,
}

impl SignerVerificationKeyStore {
    pub fn new(
        adapter: Box<dyn Adapter<Key = ProtocolPartyId, Record = ProtocolSignerVerificationKey>>,
    ) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }

    pub fn save(
        &self,
        party_id: ProtocolPartyId,
        verification_key: ProtocolSignerVerificationKey,
    ) -> Result<(), StoreError> {
        let mut adapter = self.adapter.write().unwrap();

        adapter
            .store_record(party_id, verification_key)
            .map_err(|e| StoreError::AdapterError(e.to_string()))
    }

    pub fn fetch(
        &self,
        party_id: &ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, StoreError> {
        let adapter = self.adapter.read().unwrap();
        let record = adapter
            .get_record(party_id)
            .map_err(|e| StoreError::AdapterError(e.to_string()))?;

        match record {
            Some(vk) => Ok(Some(vk.clone())),
            _ => Ok(None),
        }
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

        assert_eq!(signer_key, store.fetch(&party_id).unwrap().unwrap());
    }

    #[test]
    fn test_fetch_using_non_existent_key() {
        let (party_id, signer_key) = generate_mithril_artefacts();
        let store = SignerVerificationKeyStore::new(Box::new(DumbAdapter::new(
            party_id,
            Box::new(signer_key.clone()),
        )));

        assert!(store.fetch(&(party_id + 1)).unwrap().is_none());
    }

    #[test]
    fn test_save_verification_key() {
        let (party_id, signer_key) = generate_mithril_artefacts();
        let store = SignerVerificationKeyStore::new(Box::new(DumbAdapter::new(
            party_id + 1,
            Box::new(signer_key.clone()),
        )));

        assert!(store.fetch(&party_id).unwrap().is_none());
        assert!(store.save(party_id, signer_key).is_ok());
        assert!(store.fetch(&party_id).unwrap().is_some());
    }
}
