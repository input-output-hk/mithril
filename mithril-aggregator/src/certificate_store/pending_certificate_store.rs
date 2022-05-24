use mithril_common::entities::{Beacon, CertificatePending};
use std::sync::RwLock;
use thiserror::Error;

use super::{store_adapter::AdapterError, StoreAdapter};

#[derive(Debug, Error)]
enum StoreError {
    #[error("physical adapter returned an error: {0}")]
    AdapterError(#[from] AdapterError),
}

type Adapter = Box<dyn StoreAdapter<Key = Beacon, Record = CertificatePending>>;

struct CertificatePendingStore {
    adapter: RwLock<Adapter>,
}

impl CertificatePendingStore {
    pub fn new(adapter: RwLock<Adapter>) -> Self {
        Self { adapter }
    }

    pub fn get_last_certificate(&self) -> Result<Option<CertificatePending>, StoreError> {
        todo!()
    }

    pub fn get_from_beacon(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<CertificatePending>, StoreError> {
        todo!()
    }

    pub fn save(&mut self, _certificate: CertificatePending) -> Result<(), StoreError> {
        todo!()
    }

    pub fn get_list(&self, last_n: usize) -> Result<Vec<(Beacon, CertificatePending)>, StoreError> {
        let vars = self.adapter.read().unwrap().get_last_n_records(last_n)?;

        Ok(vars)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::certificate_store::dumb_adapter::DumbStoreAdapter;
    use mithril_common::fake_data;

    fn get_certificate_store(size: u64) -> CertificatePendingStore {
        let mut adapter: DumbStoreAdapter<Beacon, CertificatePending> = DumbStoreAdapter::new();

        for ix in 0..size {
            let beacon = Beacon::new("testnet".to_string(), ix / 3, ix);
            let certificate_pending = CertificatePending::new(
                beacon.clone(),
                fake_data::protocol_parameters(),
                ix.to_string(),
                fake_data::signers_with_stakes(5),
            );
            adapter.store_record(beacon, certificate_pending).unwrap();
        }
        let store = CertificatePendingStore::new(RwLock::new(Box::new(adapter)));

        store
    }

    #[test]
    fn list_is_empty() {
        let store = get_certificate_store(0);

        assert_eq!(0, store.get_list(100).unwrap().len());
    }

    #[test]
    fn list_has_some_members() {
        let store = get_certificate_store(1);

        assert_eq!(1, store.get_list(100).unwrap().len());
    }

    #[test]
    fn get_certificate_with_good_beacon() {
        let beacon = Beacon::new("testnet".to_string(), 0, 0);
        let store = get_certificate_store(1);
        let result = store.get_from_beacon(&beacon).unwrap();
        assert!(result.is_some());
    }
}
