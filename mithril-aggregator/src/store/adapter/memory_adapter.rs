use async_trait::async_trait;
use std::{collections::HashMap, hash::Hash};

use super::{AdapterError, StoreAdapter};

pub struct MemoryAdapter<K, V> {
    index: Vec<K>,
    values: HashMap<K, V>,
}

impl<K, V> MemoryAdapter<K, V>
where
    K: Hash + Eq + Send + Sync + Clone,
    V: Send + Sync + Clone,
{
    #[allow(dead_code)]
    pub fn new(data: Option<Vec<(K, V)>>) -> Result<Self, AdapterError> {
        let data = match data {
            None => Vec::new(),
            Some(v) => v,
        };
        let mut values = HashMap::new();
        let mut index = Vec::new();

        for (idx, elt) in data.into_iter() {
            if values.insert(idx.clone(), elt).is_some() {
                return Err(AdapterError::InitializationError(
                    "duplicate key found".into(),
                ));
            }
            index.push(idx);
        }

        Ok(Self { index, values })
    }
}

#[async_trait]
impl<K, V> StoreAdapter for MemoryAdapter<K, V>
where
    K: Hash + Eq + Send + Sync + Clone,
    V: Send + Sync + Clone,
{
    type Key = K;
    type Record = V;

    async fn store_record(
        &mut self,
        key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        let key = (*key).clone();
        let record = (*record).clone();

        if self.values.insert(key.clone(), record).is_some() {
            if let Some(ix) = self.index.iter().position(|k| k == &key) {
                let _ = self.index.remove(ix);
            } else {
                // the index was corrupted
                // doing nothing will repair it.
            }
        }
        self.index.push(key);

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        match self.values.get(key) {
            Some(val) => Ok(Some(val.clone())),
            None => Ok(None),
        }
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.values.contains_key(key))
    }

    async fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        Ok(self
            .index
            .iter()
            .rev()
            .take(how_many)
            .map(|k| (k.clone(), self.values.get(k).unwrap().clone()))
            .collect())
    }

    async fn remove(&mut self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        Ok(self.values.remove(key))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn init_adapter(nb: u64) -> MemoryAdapter<u64, String> {
        let mut values: Vec<(u64, String)> = Vec::new();
        if nb > 0 {
            for ix in 1..=nb {
                values.push((ix, format!("value {}", ix)));
            }

            MemoryAdapter::new(Some(values)).unwrap()
        } else {
            MemoryAdapter::new(None).unwrap()
        }
    }

    #[tokio::test]
    async fn record_exists_existing_key() {
        let adapter = init_adapter(2);

        assert!(adapter.record_exists(&1).await.unwrap());
    }
    #[tokio::test]
    async fn record_exists_non_existing_key() {
        let adapter = init_adapter(2);

        assert!(!adapter.record_exists(&0).await.unwrap());
    }

    #[tokio::test]
    async fn read_existing_record() {
        let adapter = init_adapter(2);
        let val = adapter.get_record(&2).await.unwrap();

        assert!(val.is_some());
        assert_eq!("value 2".to_string(), val.unwrap());
    }

    #[tokio::test]
    async fn read_unexisting_record() {
        let adapter = init_adapter(2);
        let val = adapter.get_record(&0).await.unwrap();

        assert!(val.is_none());
    }

    #[tokio::test]
    async fn get_last_n_values() {
        let adapter = init_adapter(2);
        let vals = adapter.get_last_n_records(5).await.unwrap();

        assert_eq!(2, vals.len());
        assert_eq!((2, "value 2".to_string()), vals[0]);
    }

    #[tokio::test]
    async fn get_last_n_existing_values() {
        let adapter = init_adapter(5);
        let vals = adapter.get_last_n_records(2).await.unwrap();

        assert_eq!(2, vals.len());
        assert_eq!((5, "value 5".to_string()), vals[0]);
    }

    #[tokio::test]
    async fn save_new_values() {
        let mut adapter = init_adapter(2);

        assert!(adapter.store_record(&10, &"ten".to_string()).await.is_ok());
        let vals = adapter.get_last_n_records(1).await.unwrap();

        assert_eq!((10, "ten".to_string()), vals[0]);
    }

    #[tokio::test]
    async fn update_value() {
        let mut adapter = init_adapter(2);

        assert!(adapter.store_record(&1, &"one".to_string()).await.is_ok());
        let vals = adapter.get_last_n_records(1).await.unwrap();

        assert_eq!((1, "one".to_string()), vals[0]);
    }

    #[tokio::test]
    async fn remove_existing_value() {
        let mut adapter = init_adapter(2);
        let record = adapter.remove(&1).await.unwrap().unwrap();

        assert_eq!("value 1".to_string(), record);
        assert!(!adapter.record_exists(&1).await.unwrap());
    }

    #[tokio::test]
    async fn remove_non_existing_value() {
        let mut adapter = init_adapter(2);
        let maybe_record = adapter.remove(&0).await.unwrap();

        assert!(maybe_record.is_none());
    }
}
