use std::path::Path;
use std::{
    collections::hash_map::DefaultHasher,
    fs::{self, Metadata},
    hash::{Hash, Hasher},
    io::Write,
    marker::PhantomData,
    path::PathBuf,
};

use async_trait::async_trait;
use glob::glob;
use serde::{de::DeserializeOwned, Serialize};

use super::{AdapterError, StoreAdapter};

/// A [StoreAdapter] storing data on disk serialized as json.
#[derive(Debug)]
pub struct JsonFileStoreAdapter<K, V> {
    dirpath: PathBuf,
    key: PhantomData<K>,
    value: PhantomData<V>,
}

impl<K, V> JsonFileStoreAdapter<K, V>
where
    K: Hash + PartialEq + Serialize + DeserializeOwned,
    V: Serialize + DeserializeOwned,
{
    /// JsonFileStoreAdapter factory
    pub fn new(dirpath: PathBuf) -> Result<Self, AdapterError> {
        if !dirpath.exists() {
            Self::create_dir(&dirpath)?;
        }

        Ok(Self {
            dirpath,
            key: PhantomData,
            value: PhantomData,
        })
    }

    fn create_dir(dirpath: &PathBuf) -> Result<(), AdapterError> {
        fs::create_dir_all(dirpath).map_err(|e| AdapterError::InitializationError(e.into()))?;

        Ok(())
    }

    fn get_hash_from_key(key: &K) -> String {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let checksum = hasher.finish();

        format!("{:0>16X}", checksum)
    }

    fn get_filename_from_key(&self, key: &K) -> PathBuf {
        let hash = Self::get_hash_from_key(key);
        let filename = format!("{}.json", hash);

        self.dirpath.join(filename)
    }

    fn get_hash_iter(&self) -> Result<Box<dyn Iterator<Item = String>>, AdapterError> {
        let mut hashes: Vec<(String, Metadata)> = Vec::new();
        let glob_expr = format!("{}/*.key", &self.dirpath.to_str().unwrap());

        for entry in glob(&glob_expr).map_err(|e| AdapterError::OpeningStreamError(e.into()))? {
            let path = entry.map_err(|e| AdapterError::OpeningStreamError(e.into()))?;
            let metadata =
                fs::metadata(&path).map_err(|e| AdapterError::OpeningStreamError(e.into()))?;
            hashes.push((
                path.as_path()
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
                metadata,
            ));
        }
        hashes.sort_by_key(|(_, meta)| meta.modified().unwrap());

        Ok(Box::new(hashes.into_iter().rev().map(|(hash, _meta)| hash)))
    }

    fn write_file(&self, filename: &str, msg: &str) -> Result<(), AdapterError> {
        let filepath = self.dirpath.join(filename);
        let mut file =
            fs::File::create(filepath).map_err(|e| AdapterError::MutationError(e.into()))?;
        file.write_fmt(format_args!("{}", msg))
            .map_err(|e| AdapterError::MutationError(e.into()))?;

        Ok(())
    }

    fn create_record(&self, key: &K, value: &V) -> Result<(), AdapterError> {
        let hash = Self::get_hash_from_key(key);
        let key_filename = format!("{}.key", hash);
        self.write_file(
            &key_filename,
            &serde_json::to_string(key).map_err(|e| AdapterError::MutationError(e.into()))?,
        )?;
        self.update_record(key, value)?;

        Ok(())
    }

    fn update_record(&self, key: &K, value: &V) -> Result<(), AdapterError> {
        let hash = Self::get_hash_from_key(key);
        let filename = format!("{}.json", hash);
        self.write_file(
            &filename,
            &serde_json::to_string(value).map_err(|e| AdapterError::MutationError(e.into()))?,
        )
    }

    /// Read the record at the given path, returns None if the file doesn't exist
    fn read_record(&self, filepath: &Path) -> Result<Option<V>, AdapterError> {
        if !filepath.is_file() {
            return Ok(None);
        }

        let value =
            fs::read_to_string(filepath).map_err(|e| AdapterError::OpeningStreamError(e.into()))?;
        let record: V =
            serde_json::from_str(&value).map_err(|e| AdapterError::ParsingDataError(e.into()))?;

        Ok(Some(record))
    }
}

#[async_trait]
impl<K, V> StoreAdapter for JsonFileStoreAdapter<K, V>
where
    K: Hash + PartialEq + Serialize + DeserializeOwned + Sync + Send,
    V: Serialize + DeserializeOwned + Sync + Send,
{
    type Key = K;
    type Record = V;

    /// Create (or update) a Value in the Store.
    /// When it is created, a key file with the same Hash as the value which
    /// contains the actual key the value is associated with.
    async fn store_record(
        &mut self,
        key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        if self.record_exists(key).await? {
            self.update_record(key, record)
        } else {
            self.create_record(key, record)
        }
    }

    /// Find and returns the expected value from its Key hash
    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        let filepath = self.get_filename_from_key(key);
        let record = self.read_record(&filepath)?;

        Ok(record)
    }

    /// Simple implementation
    /// If the file exists, then the document exists
    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.get_filename_from_key(key).is_file())
    }

    /// Get last N records is fairly complex, read the directory to return the
    /// most recent elements only. This implies being able to sort on creation?
    /// modification? date and be able to get the Key (not its hash)
    async fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        let hashes: Vec<String> = self.get_hash_iter()?.take(how_many).collect();
        let mut records: Vec<(K, V)> = vec![];

        for hash in hashes {
            let filename = format!("{}.key", hash);
            let content = fs::read_to_string(self.dirpath.join(filename))
                .map_err(|e| AdapterError::OpeningStreamError(e.into()))?;
            let key: K = serde_json::from_str(&content)
                .map_err(|e| AdapterError::ParsingDataError(e.into()))?;
            let record = self.get_record(&key).await?.unwrap();
            // panic if no value file is associated to the key
            // remove offending key if this case occures
            records.push((key, record));
        }

        Ok(records)
    }

    async fn remove(&mut self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        if let Some(value) = self.get_record(key).await? {
            let path = self.get_filename_from_key(key);
            fs::remove_file(path).map_err(|e| AdapterError::MutationError(e.into()))?;

            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError> {
        let hashes = self.get_hash_iter()?;
        Ok(Box::new(hashes.map(|hash| {
            let filepath = self.dirpath.join(format!("{}.json", hash));
            self.read_record(&filepath).unwrap().unwrap()
        })))
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;
    use std::borrow::Borrow;
    use std::path::Path;
    use std::time::Duration;

    use super::*;

    fn get_adapter(dir_name: &str) -> (PathBuf, JsonFileStoreAdapter<u64, String>) {
        let dir = std::env::temp_dir().join("mithril_test").join(dir_name);

        if dir.exists() {
            let _ = fs::remove_dir_all(&dir);
        }

        (dir.clone(), JsonFileStoreAdapter::new(dir).unwrap())
    }

    fn init_dir(dir: &Path) {
        for (idx, hash, msg) in [
            (1, "1E9F734161D62DD9", "one"),
            (2, "A4D31070D122B816", "two"),
            (3, "FD8C671699409CFA", "three"),
            (4, "55D754F7C9ED6591", "four"),
            (5, "DB30EB0519197F1C", "five"),
            (6, "FBD8F25EE5D0C5A8", "six"),
        ] {
            let mut file = fs::File::create(dir.join(format!("{}.json", hash))).unwrap();
            let value = json!(msg);
            file.write_fmt(format_args!("{}", value)).unwrap();
            let mut file = fs::File::create(dir.join(format!("{}.key", hash))).unwrap();
            let value = json!(idx);
            file.write_fmt(format_args!("{}", value)).unwrap();
            std::thread::sleep(Duration::from_millis(100));
        }
    }

    #[tokio::test]
    async fn check_file_exists() {
        let (dir, adapter) = get_adapter("check_file_exists");
        init_dir(&dir);
        assert!(adapter.record_exists(&1).await.unwrap());
    }

    #[tokio::test]
    async fn check_file_does_not_exist() {
        let (dir, adapter) = get_adapter("check_file_does_not_exist");
        init_dir(&dir);
        assert!(
            !adapter.record_exists(&8).await.unwrap(),
            "A record for key `8` should not exit"
        );
    }

    #[tokio::test]
    async fn check_get_record() {
        let (dir, adapter) = get_adapter("check_get_record");
        init_dir(&dir);
        let content = adapter.get_record(&1).await.unwrap().unwrap();
        assert_eq!("one", content);
    }

    #[tokio::test]
    async fn check_get_last_n() {
        let (dir, adapter) = get_adapter("check_get_last_n");
        init_dir(&dir);
        let values = adapter.get_last_n_records(2).await.unwrap();
        assert_eq!(
            vec![(6, "six".to_string()), (5, "five".to_string())],
            values
        );
    }

    #[tokio::test]
    async fn check_get_last_n_modified_records() {
        let (dir, mut adapter) = get_adapter("check_get_last_n");
        init_dir(&dir);
        adapter
            .store_record(&4, "updated record".to_string().borrow())
            .await
            .unwrap();
        let values = adapter.get_last_n_records(2).await.unwrap();
        assert_eq!(
            vec![(6, "six".to_string()), (5, "five".to_string())],
            values
        );
    }

    #[tokio::test]
    async fn check_create_record() {
        let (_dir, mut adapter) = get_adapter("check_create_record");
        let record = "just one".to_string();

        assert!(adapter.store_record(&1, &record).await.is_ok());
        assert_eq!(record, adapter.get_record(&1).await.unwrap().unwrap());
    }

    #[tokio::test]
    async fn check_update_record() {
        let (dir, mut adapter) = get_adapter("check_update_record");
        init_dir(&dir);
        let record = "just one".to_string();

        assert!(adapter.store_record(&1, &record).await.is_ok());
        assert_eq!(record, adapter.get_record(&1).await.unwrap().unwrap());
    }

    #[tokio::test]
    async fn check_remove_existing_record() {
        let (dir, mut adapter) = get_adapter("check_remove_existing_record");
        init_dir(&dir);
        let value = adapter.remove(&1).await.unwrap().unwrap();

        assert_eq!("one".to_string(), value);
        assert!(!adapter.record_exists(&1).await.unwrap());
    }

    #[tokio::test]
    async fn check_remove_non_existing_record() {
        let (dir, mut adapter) = get_adapter("check_remove_non_existing_record");
        init_dir(&dir);
        let maybe_value = adapter.remove(&0).await.unwrap();

        assert!(maybe_value.is_none());
    }

    #[tokio::test]
    async fn remove_non_existing_value() {
        let (_dir, mut adapter) = get_adapter("remove_non_existing_value");
        let maybe_record = adapter.remove(&0).await.unwrap();

        assert!(maybe_record.is_none());
    }

    #[tokio::test]
    async fn test_iter_record() {
        let (dir, adapter) = get_adapter("test_iter_record");
        init_dir(&dir);
        let records: Vec<String> = adapter.get_iter().await.unwrap().collect();

        assert_eq!(vec!["six", "five", "four", "three", "two", "one"], records);
    }

    #[tokio::test]
    async fn test_iter_without_record() {
        let (_dir, adapter) = get_adapter("test_iter_without_record");
        let records = adapter.get_iter().await.unwrap();

        assert_eq!(0, records.count());
    }
}
