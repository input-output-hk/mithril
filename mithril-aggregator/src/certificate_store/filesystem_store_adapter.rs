use std::path::PathBuf;

use crate::MultiSignerImpl;

use super::{AdapterError, StoreAdapter};

struct FilesystemStoreAdapter {
    store_dir: PathBuf,
}

impl StoreAdapter for FilesystemStoreAdapter {
    type Key = u64;
    type Record = MultiSignerImpl;

    fn store_record(&mut self, _key: Self::Key, _record: Self::Record) -> Result<(), AdapterError> {
        todo!()
    }

    fn get_record(&self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        todo!()
    }

    fn record_exists(&self, _key: &Self::Key) -> Result<bool, AdapterError> {
        todo!()
    }

    fn get_last_n_records(
        &self,
        _how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {}
