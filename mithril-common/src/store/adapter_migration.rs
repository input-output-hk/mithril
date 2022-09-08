//! Data adapter migration module.
//!
use std::{error::Error, hash::Hash};

use crate::store::adapter::{JsonFileStoreAdapter, SQLiteAdapter, StoreAdapter};
use serde::{de::DeserializeOwned, Serialize};

/// Migrate data from an adapter to another.
/// Adapters must use the same Key & Value types.
///
/// Here is an example of migration from JSON files to SQLite3 store adapter:
/// ```
/// use std::path::PathBuf;
/// use mithril_common::store::adapter_migration::AdapterMigration;
/// use mithril_common::store::adapter::{JsonFileStoreAdapter, SQLiteAdapter};
///
/// async fn migrate() -> Result<(), Box<dyn std::error::Error>> {
///     let source_adapter: JsonFileStoreAdapter<u64, String> = JsonFileStoreAdapter::new(PathBuf::new().join("dirpath"))?;
///     let destination_adapter: SQLiteAdapter<u64, String> = SQLiteAdapter::new("table",Some(PathBuf::new().join("file.sqlite3")))?;
///     let mut migration = AdapterMigration::new(source_adapter, destination_adapter);

///     migration.migrate().await
/// }
/// ```
pub struct AdapterMigration<K, V> {
    adapter_from: JsonFileStoreAdapter<K, V>,
    adapter_to: SQLiteAdapter<K, V>,
}

impl<K, V> AdapterMigration<K, V>
where
    K: PartialEq + Clone + Serialize + DeserializeOwned + Sync + Send + Hash,
    V: Clone + Serialize + DeserializeOwned + Sync + Send,
{
    /// Create a new instance.
    pub fn new(adapter_from: JsonFileStoreAdapter<K, V>, adapter_to: SQLiteAdapter<K, V>) -> Self {
        Self {
            adapter_from,
            adapter_to,
        }
    }

    /// Launch migration process. This will copy all data from source adapter to destination adapter.
    pub async fn migrate(&mut self) -> Result<(), Box<dyn Error>> {
        for (key, record) in self
            .adapter_from
            .get_last_n_records(usize::MAX)
            .await?
            .into_iter()
            .rev()
        {
            self.adapter_to.store_record(&key, &record).await?;
        }

        Ok(())
    }

    /// Check if data are the same in both source & destination adapters.
    pub async fn check(&self) -> Result<bool, Box<dyn Error>> {
        let from_data: Vec<(K, String)> = self
            .adapter_from
            .get_last_n_records(usize::MAX)
            .await?
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    serde_json::to_string(&v).unwrap_or_else(|_| String::new()),
                )
            })
            .collect();
        let to_data: Vec<(K, String)> = self
            .adapter_to
            .get_last_n_records(usize::MAX)
            .await?
            .into_iter()
            .map(|(k, v)| {
                (
                    k,
                    serde_json::to_string(&v).unwrap_or_else(|_| String::new()),
                )
            })
            .collect();

        Ok(from_data == to_data)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf, time::Duration};

    use tokio::time::sleep;

    use super::*;

    fn get_adapter(dir_name: &str) -> (PathBuf, JsonFileStoreAdapter<u64, String>) {
        let dir = std::env::temp_dir().join("mithril_test").join(dir_name);

        if dir.exists() {
            let _ = fs::remove_dir_all(&dir);
        }

        (dir.clone(), JsonFileStoreAdapter::new(dir).unwrap())
    }

    async fn fill_adapter<K, R>(
        adapter: &mut dyn StoreAdapter<Key = K, Record = R>,
        values: &[(K, R)],
    ) {
        for (key, record) in values {
            adapter.store_record(key, record).await.unwrap();
            sleep(Duration::from_millis(100)).await;
        }
    }

    #[tokio::test]
    async fn test_migration() {
        let (dir, mut json_adapter) = get_adapter("test_sqlite_migration");
        let sqlite_file = dir.clone().join("test.sqlite3");
        let expected = vec![
            (1_u64, "one".to_string()),
            (2, "two".to_string()),
            (3, "three".to_string()),
            (4, "four".to_string()),
        ];

        fill_adapter(&mut json_adapter, &expected).await;

        {
            let sqlite_adapter =
                SQLiteAdapter::new("test_migration", Some(sqlite_file.clone())).unwrap();
            let mut migration = AdapterMigration::new(json_adapter, sqlite_adapter);
            migration.migrate().await.unwrap();
        }

        let sqlite_adapter: SQLiteAdapter<u64, String> =
            SQLiteAdapter::new("test_migration", Some(sqlite_file.clone())).unwrap();
        let collected: Vec<(u64, String)> = sqlite_adapter
            .get_last_n_records(usize::MAX)
            .await
            .unwrap()
            .into_iter()
            .rev()
            .collect();

        assert_eq!(expected, collected);
    }

    #[tokio::test]
    async fn test_check_ok() {
        let expected = vec![
            (1_u64, "one".to_string()),
            (2, "two".to_string()),
            (3, "three".to_string()),
            (4, "four".to_string()),
        ];
        let (_dir, mut json_adapter) = get_adapter("test_sqlite_migration");
        let mut sqlite_adapter = SQLiteAdapter::new("test_migration", None).unwrap();

        fill_adapter(&mut json_adapter, &expected).await;
        fill_adapter(&mut sqlite_adapter, &expected).await;

        let migration = AdapterMigration::new(json_adapter, sqlite_adapter);
        assert!(migration.check().await.unwrap());
    }

    #[tokio::test]
    async fn test_check_not_ok() {
        let expected = vec![
            (1_u64, "one".to_string()),
            (2, "two".to_string()),
            (3, "three".to_string()),
            (4, "four".to_string()),
        ];
        let (_dir, mut json_adapter) = get_adapter("test_sqlite_migration");
        let mut sqlite_adapter = SQLiteAdapter::new("test_migration", None).unwrap();

        fill_adapter(&mut json_adapter, &expected).await;
        fill_adapter(&mut sqlite_adapter, &expected[..3]).await;

        let migration = AdapterMigration::new(json_adapter, sqlite_adapter);
        assert!(!migration.check().await.unwrap());
    }
}
