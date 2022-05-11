#![allow(dead_code, unused_variables)]
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
        todo!()
    }

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.data.contains_key(key))
    }

    fn store_record(&mut self, key: Self::Key, record: Self::Record) -> Result<(), AdapterError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn store_record_works() {
        let mut adapter: MemoryAdapter<u64, &str> = MemoryAdapter::new();

        assert!(!adapter.record_exists(&0).unwrap());
        assert!(adapter.store_record(0, "zero").is_ok());
        assert!(adapter.record_exists(&0).unwrap());
    }

    #[test]
    fn record_exists_works() {
        let data: HashMap<u64, &str> = {
            let mut data: HashMap<u64, &str> = HashMap::new();
            data.insert(1, "one");
            data.insert(2, "two");

            data
        };
        let adapter: MemoryAdapter<u64, &str> = MemoryAdapter::new_with(data);

        assert!(adapter.record_exists(&1).unwrap());
        assert!(adapter.record_exists(&2).unwrap());
        assert!(!adapter.record_exists(&3).unwrap());
    }
}
