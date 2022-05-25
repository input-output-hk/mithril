use std::{
    collections::hash_map::DefaultHasher,
    fs::File,
    hash::{Hash, Hasher},
    marker::PhantomData,
    path::PathBuf,
};

use serde::{de::DeserializeOwned, Serialize};
use serde_json::json;

use super::{AdapterError, StoreAdapter};

struct JsonFileStoreAdapter<K, V> {
    dirpath: PathBuf,
    key: PhantomData<K>,
    value: PhantomData<V>,
}

impl<K, V> JsonFileStoreAdapter<K, V>
where
    K: Hash + PartialEq,
    V: Serialize + DeserializeOwned,
{
    fn create_dir(dirpath: &PathBuf) -> Result<(), AdapterError> {
        std::fs::create_dir_all(dirpath)
            .map_err(|e| AdapterError::InitializationError(e.into()))?;

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

    fn get_filename_from_key(&self, key: &K) -> PathBuf {
        let filename = {
            let mut hasher = DefaultHasher::new();
            key.hash(&mut hasher);
            hasher.finish()
        };
        let filename = format!("{}.json", filename);

        self.dirpath.join(filename)
    }
}

impl<K, V> StoreAdapter for JsonFileStoreAdapter<K, V>
where
    K: Hash + PartialEq,
    V: Serialize + DeserializeOwned,
{
    type Key = K;
    type Record = V;

    fn store_record(&mut self, _key: Self::Key, _record: Self::Record) -> Result<(), AdapterError> {
        todo!()
    }

    fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        if !self.record_exists(key)? {
            return Ok(None);
        }
        let filepath = self.get_filename_from_key(key);
        let value = std::fs::read_to_string(filepath)
            .map_err(|e| AdapterError::OpeningStreamError(e.into()))?;
        let record: V =
            serde_json::from_str(&value).map_err(|e| AdapterError::ParsingDataError(e.into()))?;

        Ok(Some(record))
    }

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.get_filename_from_key(key).is_file())
    }

    fn get_last_n_records(
        &self,
        _how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

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
        let _ = File::create(dir.join("2206609067086327257.json")).unwrap();
        assert!(adapter.record_exists(&1).unwrap());
        rmdir(dir);
    }

    #[test]
    fn check_file_does_not_exist() {
        let dir = get_pathbuf().join("check_file_does_not_exist");
        let adapter = get_adapter(&dir);
        assert!(!adapter.record_exists(&1).unwrap());
        rmdir(dir);
    }

    #[test]
    fn check_get_record() {
        let dir = get_pathbuf().join("check_get_record");
        let adapter = get_adapter(&dir);
        let mut file = File::create(dir.join("2206609067086327257.json")).unwrap();
        let value = json!("content");
        file.write_fmt(format_args!("{}", value)).unwrap();
        let content = adapter.get_record(&1).unwrap().unwrap();
        assert_eq!("content", content);
        rmdir(dir);
    }
}
