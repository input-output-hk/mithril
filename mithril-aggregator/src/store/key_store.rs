#![allow(dead_code)]
use super::adapters::Adapter;
use super::AdapterError;
use mithril_common::crypto_helper::{ProtocolPartyId, ProtocolSignerVerificationKey};
use std::sync::RwLock;

struct SignerVerificationKeyStore {
    adapter:
        Box<RwLock<dyn Adapter<Key = ProtocolPartyId, Record = ProtocolSignerVerificationKey>>>,
}

impl SignerVerificationKeyStore {
    pub fn new(
        adapter: Box<
            RwLock<dyn Adapter<Key = ProtocolPartyId, Record = ProtocolSignerVerificationKey>>,
        >,
    ) -> Self {
        Self { adapter }
    }

    pub fn save(
        &self,
        party_id: ProtocolPartyId,
        verification_key: ProtocolSignerVerificationKey,
    ) -> Result<(), AdapterError> {
        let mut adapter = self
            .adapter
            .write()
            .map_err(|e| AdapterError::InputOutputError(e.to_string()))?;

        adapter.store_record(party_id, verification_key)
    }

    /// returns a verification key matching the given party_id
    /// it actually returns a clone of the matching verification key if any this
    /// is because a lock is used to allow concurrency use of this
    /// store, it is necessary to release the lock before returning the value
    pub fn fetch(
        &self,
        party_id: ProtocolPartyId,
    ) -> Result<Option<ProtocolSignerVerificationKey>, AdapterError> {
        let adapter = self.adapter.read().unwrap();
        let record = adapter
            .get_record(&party_id)
            .map_err(|e| AdapterError::InputOutputError(e.to_string()))?;

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

    fn instanciate_populated_store() -> (
        ProtocolPartyId,
        ProtocolSignerVerificationKey,
        SignerVerificationKeyStore,
    ) {
        let (party_id, signer_key) = generate_mithril_artefacts();
        let store = SignerVerificationKeyStore::new(Box::new(RwLock::new(DumbAdapter::new(
            party_id,
            Box::new(signer_key.clone()),
        ))));

        (party_id, signer_key, store)
    }

    #[test]
    fn test_fetch_verification_key() {
        let (party_id, signer_key, store) = instanciate_populated_store();

        assert_eq!(signer_key, store.fetch(party_id).unwrap().unwrap());
    }

    #[test]
    fn test_fetch_using_non_existent_key() {
        let (party_id, _signer_key, store) = instanciate_populated_store();

        assert!(store.fetch(party_id + 1).unwrap().is_none());
    }

    #[test]
    fn test_save_verification_key() {
        let (party_id, signer_key, store) = instanciate_populated_store();

        assert!(store.fetch(party_id + 1).unwrap().is_none());
        assert!(store.save(party_id + 1, signer_key).is_ok());
        assert!(store.fetch(party_id + 1).unwrap().is_some());
    }
}
