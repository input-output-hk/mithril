//! Data adapter migration module.
//!
use std::{
    collections::HashSet,
    error::Error,
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::store::adapter::{JsonFileStoreAdapter, SQLiteAdapter, StoreAdapter};
use serde::{de::DeserializeOwned, Serialize};

/// Migration check outcome.
#[derive(Debug, PartialEq)]
pub enum MigrationCheckOutcome<K, V> {
    /// Values and keys are equal and key order is preserved.
    Equal,
    /// Source and target do not have the same key list.
    KeysMismatch {
        /// List of keys present in source but not in target.
        source: Vec<K>,
        /// List of keys present in target but not in source.
        target: Vec<K>,
    },
    /// For the given key, value in source is different from target.
    ValueNotEqualInTarget {
        /// Key for which values are different in both adapters.
        key: K,
        /// Value in the source adapter.
        source: V,
        /// Value in the target adapter.
        target: V,
    },
    /// Keys in target is not in the same order than in source.
    KeyNotAligned {
        /// Expected key as read from the source adapter.
        source: K,
        /// Key found in the target adapter.
        target: K,
    },
}

impl<K, V> MigrationCheckOutcome<K, V> {
    /// Return true if the stores are the same, false otherwise.
    pub fn is_ok(&self) -> bool {
        matches!(*self, Self::Equal)
    }
}

impl<K, V> Display for MigrationCheckOutcome<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equal => write!(f, "Adapters have the same data."),
            Self::KeysMismatch { source, target } => write!(
                f,
                "{} keys are only in source and {} keys are only in target.",
                source.len(),
                target.len()
            ),
            Self::KeyNotAligned {
                source: _,
                target: _,
            } => {
                write!(f, "Keys in source and target are unaligned.")
            }
            Self::ValueNotEqualInTarget {
                key: _,
                source: _,
                target: _,
            } => write!(f, "Data mismatch in source and target."),
        }
    }
}

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
    K: Eq + Clone + Serialize + DeserializeOwned + Sync + Send + Hash,
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
    pub async fn check(&self) -> Result<MigrationCheckOutcome<K, V>, Box<dyn Error>> {
        let from_data: Vec<(K, V)> = self.adapter_from.get_last_n_records(usize::MAX).await?;
        let to_data: Vec<(K, V)> = self.adapter_to.get_last_n_records(usize::MAX).await?;
        {
            let from_data: HashSet<K> = from_data.iter().map(|(k, _v)| k).cloned().collect();
            let to_data: HashSet<K> = to_data.iter().map(|(k, _v)| k).cloned().collect();
            let source_diff: Vec<K> = from_data.difference(&to_data).cloned().collect();
            let target_diff: Vec<K> = to_data.difference(&from_data).cloned().collect();

            if source_diff.len() > 0 || target_diff.len() > 0 {
                return Ok(MigrationCheckOutcome::KeysMismatch {
                    source: source_diff,
                    target: target_diff,
                });
            }
        }

        for ((k_from, _v_from), (k_to, _v_to)) in from_data.into_iter().zip(to_data.into_iter()) {
            if k_from != k_to {
                return Ok(MigrationCheckOutcome::KeyNotAligned {
                    source: k_from,
                    target: k_to,
                });
            }
        }

        Ok(MigrationCheckOutcome::Equal)
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
        assert!(migration.check().await.unwrap().is_ok());
    }

    #[tokio::test]
    async fn test_check_key_not_exist() {
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
        let outcome = migration.check().await.unwrap();
        assert!(!outcome.is_ok());

        assert_eq!(
            MigrationCheckOutcome::KeysMismatch {
                source: vec![4],
                target: Vec::new()
            },
            outcome
        );
    }

    #[tokio::test]
    async fn test_check_keys_unaligned() {
        let expected = vec![
            (1_u64, "one".to_string()),
            (2, "two".to_string()),
            (3, "three".to_string()),
            (4, "four".to_string()),
        ];
        let (_dir, mut json_adapter) = get_adapter("test_sqlite_migration");
        let mut sqlite_adapter = SQLiteAdapter::new("test_migration", None).unwrap();

        fill_adapter(&mut json_adapter, &expected).await;
        // reversing collection in target to unalign keys
        fill_adapter(
            &mut sqlite_adapter,
            &expected.into_iter().rev().collect::<Vec<(u64, String)>>(),
        )
        .await;
        let migration = AdapterMigration::new(json_adapter, sqlite_adapter);
        let outcome = migration.check().await.unwrap();
        assert!(!outcome.is_ok());

        assert_eq!(
            MigrationCheckOutcome::KeyNotAligned {
                source: 4,
                target: 1
            },
            outcome
        );
    }

    #[tokio::test]
    async fn test_check_different_values() {
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
        // changing one value in destination
        sqlite_adapter
            .store_record(&2, &"not two".to_string())
            .await
            .unwrap();
        let migration = AdapterMigration::new(json_adapter, sqlite_adapter);
        let outcome = migration.check().await.unwrap();
        assert!(!outcome.is_ok());

        assert_eq!(
            MigrationCheckOutcome::ValueNotEqualInTarget {
                key: 2,
                source: "two".to_string(),
                target: "not two".to_string()
            },
            outcome
        );
    }
}
