use async_trait::async_trait;
use tokio::sync::RwLock;

use super::{adapter::StoreAdapter, StoreError};

/// Implementing this trait will make store able to limit the number of the
/// stored records by pruning them if a limit is set.
#[async_trait]
pub trait StorePruner {
    /// The key type
    type Key: Sync + Send;

    /// The record type
    type Record: Sync + Send;

    /// This trait requires a way to get the internal adapter.
    fn get_adapter(&self)
        -> &RwLock<Box<dyn StoreAdapter<Key = Self::Key, Record = Self::Record>>>;

    /// Return the maximum number of elements that can exist in this store. If None, there is no limit.
    fn get_max_records(&self) -> Option<usize>;

    /// Prune elements exceeding the specified limit.
    async fn prune(&self) -> Result<(), StoreError> {
        let retention_len = self.get_max_records().unwrap_or(usize::MAX);
        let lock = self.get_adapter();
        let mut adapter = lock.write().await;

        for (epoch, _record) in adapter
            .get_last_n_records(usize::MAX)
            .await?
            .into_iter()
            .skip(retention_len)
        {
            adapter.remove(&epoch).await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{cmp::min, sync::Arc};

    use sqlite::Connection;
    use tokio::sync::Mutex;

    use crate::store::adapter::SQLiteAdapter;

    use super::*;

    struct TestStore {
        adapter: RwLock<Box<dyn StoreAdapter<Key = u64, Record = String>>>,
        record_limit: Option<usize>,
    }

    impl StorePruner for TestStore {
        type Key = u64;
        type Record = String;

        fn get_adapter(
            &self,
        ) -> &RwLock<Box<dyn StoreAdapter<Key = Self::Key, Record = Self::Record>>> {
            &self.adapter
        }

        fn get_max_records(&self) -> Option<usize> {
            self.record_limit
        }
    }

    fn get_data(n: u64) -> Vec<(u64, String)> {
        let n = min(n, 6);
        let words = ["one", "two", "three", "four", "five", "six"];
        let mut values: Vec<(u64, String)> = Vec::new();

        for index in 0..n {
            values.push((index, words[index as usize].to_string()));
        }

        values
    }

    async fn get_adapter(data_len: u64) -> SQLiteAdapter<u64, String> {
        let connection = Arc::new(Mutex::new(Connection::open(":memory:").unwrap()));
        let mut adapter: SQLiteAdapter<u64, String> =
            SQLiteAdapter::new("whatever", connection).unwrap();

        for (key, record) in get_data(data_len) {
            adapter.store_record(&key, &record).await.unwrap();
        }

        adapter
    }

    #[tokio::test]
    async fn test_no_prune() {
        for data_len in 1_u64..=6 {
            let store = TestStore {
                adapter: RwLock::new(Box::new(get_adapter(data_len).await)),
                record_limit: None,
            };

            store.prune().await.unwrap();
            assert_eq!(
                data_len as usize,
                store.adapter.read().await.get_iter().await.unwrap().count(),
                "test no pruning with dataset length = {data_len}"
            );
        }
    }
    #[tokio::test]
    async fn test_with_pruning() {
        for data_len in 1_u64..=6 {
            let store = TestStore {
                adapter: RwLock::new(Box::new(get_adapter(6).await)),
                record_limit: Some(data_len as usize),
            };

            store.prune().await.unwrap();
            assert_eq!(
                data_len as usize,
                store.adapter.read().await.get_iter().await.unwrap().count(),
                "test pruning with retention limit = {data_len}"
            );
        }
    }
}
