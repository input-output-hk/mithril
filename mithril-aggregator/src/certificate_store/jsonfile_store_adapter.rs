use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    marker::PhantomData,
    path::PathBuf,
};

use serde::Serialize;
use warp::reply::Json;

use super::{AdapterError, StoreAdapter};

struct JsonFileStoreAdapter<K, V> {
    dirpath: PathBuf,
    key: PhantomData<K>,
    value: PhantomData<V>,
}

impl<K, V> JsonFileStoreAdapter<K, V> {
    fn create_dir(dirpath: &PathBuf) -> Result<(), AdapterError> {
        std::fs::create_dir_all(dirpath).map_err(|e| {
            AdapterError::InitializationError(format!(
                "{} → {}",
                e.to_string(),
                dirpath.to_str().unwrap()
            ))
        })?;

        Ok(())
    }
    pub fn new(dirpath: PathBuf) -> Result<Self, AdapterError> {
        if !dirpath.exists() {
            Self::create_dir(&dirpath)?;
        }

        Ok(Self {
            dirpath: dirpath,
            key: PhantomData,
            value: PhantomData,
        })
    }
}

impl<K, V> StoreAdapter for JsonFileStoreAdapter<K, V>
where
    K: Hash + PartialEq,
    V: Serialize,
{
    type Key = K;
    type Record = V;

    fn store_record(&mut self, _key: Self::Key, _record: Self::Record) -> Result<(), AdapterError> {
        todo!()
    }

    fn get_record(&self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        todo!()
    }

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        let filename = {
            let mut hasher = DefaultHasher::new();
            key.hash(&mut hasher);
            hasher.finish()
        };
        let filename = format!("{}.json", filename);
        println!("checking file {}", filename);

        Ok(self.dirpath.join(filename).is_file())
    }

    fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_adapter(dir: &PathBuf) -> JsonFileStoreAdapter<u64, String> {
        JsonFileStoreAdapter::new((*dir).clone()).unwrap()
    }

    fn get_pathbuf() -> PathBuf {
        std::env::temp_dir().join("mithril_test")
    }

    fn rmdir(dir: PathBuf) {
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn check_file_exists() {
        let dir = get_pathbuf().join("check_file_exists");
        let adapter = get_adapter(&dir);
        let _ = std::fs::File::create(dir.join("2206609067086327257.json")).unwrap();
        assert!(adapter.record_exists(&1).unwrap());
        rmdir(dir);
    }
}
