use async_trait::async_trait;
use std::{collections::HashMap, hash::Hash, sync::Arc};

use super::{AdapterError, StoreAdapter};

pub struct MemoryAdapter<K, V> {
    index: Vec<Arc<K>>,
    values: HashMap<Arc<K>, V>,
}

impl<K, V> MemoryAdapter<K, V>
where
    K: Hash + Eq + Send + Sync,
    V: Send + Sync,
{
    #[allow(dead_code)]
    pub fn new(data: Option<Vec<(K, V)>>) -> Result<Self, AdapterError> {
        let data = if data.is_none() {
            Vec::new()
        } else {
            data.unwrap()
        };
        let mut values = HashMap::new();
        let mut index = Vec::new();

        for (idx, elt) in data.into_iter() {
            let idx = Arc::new(idx);

            if values.insert(idx.clone(), elt).is_some() {
                return Err(AdapterError::InitializationError(
                    format!("duplicate key found").into(),
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
    K: Hash + Eq + Send + Sync,
    V: Send + Sync,
{
    type Key = K;
    type Record = V;

    async fn store_record(
        &mut self,
        _key: &Self::Key,
        _record: &Self::Record,
    ) -> Result<(), AdapterError> {
        todo!()
    }

    async fn get_record(&self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        todo!()
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.values.contains_key(key))
    }

    async fn get_last_n_records(
        &self,
        _how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        todo!()
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
}
