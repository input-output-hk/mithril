#![allow(dead_code)]
use std::{collections::HashMap, hash::Hash};

use super::{Adapter, AdapterError};

pub struct MemoryAdapter<I: std::cmp::PartialEq, V> {
    data: HashMap<I, V>,
}

impl<I, V> MemoryAdapter<I, V>
where
    I: std::cmp::PartialEq,
{
    pub fn new() -> Self {
        Self::new_with(HashMap::new())
    }

    pub fn new_with(data: HashMap<I, V>) -> Self {
        Self { data }
    }
}

impl<I, V> Adapter for MemoryAdapter<I, V>
where
    I: std::cmp::Eq,
    I: Hash,
{
    type Key = I;
    type Record = V;

    fn get_record(&self, key: &Self::Key) -> Result<Option<&Self::Record>, AdapterError> {
        Ok(self.data.get(key))
    }

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.data.contains_key(key))
    }

    fn store_record(&mut self, key: Self::Key, record: Self::Record) -> Result<(), AdapterError> {
        let _old_value = self.data.insert(key, record);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn instanciate_populated_store() -> MemoryAdapter<u64, String> {
        let data: HashMap<u64, String> = {
            let mut data: HashMap<u64, String> = HashMap::new();
            data.insert(1, "one".to_string());
            data.insert(2, "two".to_string());

            data
        };

        MemoryAdapter::new_with(data)
    }

    #[test]
    fn store_record_works() {
        let mut adapter: MemoryAdapter<u64, &str> = MemoryAdapter::new();

        assert!(!adapter.record_exists(&0).unwrap());
        assert!(adapter.store_record(0, "zero").is_ok());
        assert!(adapter.record_exists(&0).unwrap());
    }

    #[test]
    fn record_exists_works() {
        let adapter = instanciate_populated_store();

        assert!(adapter.record_exists(&1).unwrap());
        assert!(adapter.record_exists(&2).unwrap());
        assert!(!adapter.record_exists(&3).unwrap());
    }

    #[test]
    fn get_record_with_existing_key_work() {
        let adapter = instanciate_populated_store();
        let value = adapter.get_record(&1).unwrap();

        assert!(value.is_some());
        let value = value.unwrap();

        assert_eq!(&("one".to_string()), value);
    }

    #[test]
    fn get_record_with_non_existing_key_work() {
        let adapter = instanciate_populated_store();
        let value = adapter.get_record(&0).unwrap();

        assert!(value.is_none());
    }
}
