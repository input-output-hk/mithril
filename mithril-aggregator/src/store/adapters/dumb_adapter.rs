#![allow(dead_code)]
use super::{Adapter, AdapterError};

// DumbAdapter stores a single value for testing purposes.
pub struct DumbAdapter<I: std::cmp::PartialEq, V> {
    key: I,
    value: V,
}

impl<I, V> DumbAdapter<I, V>
where
    I: std::cmp::PartialEq,
{
    pub fn new(key: I, value: Box<V>) -> Self {
        let value = *value;

        Self { key, value }
    }
}

impl<I, V> Adapter for DumbAdapter<I, V>
where
    I: std::cmp::PartialEq,
{
    type Key = I;
    type Record = V;

    fn get_record(&self, key: &Self::Key) -> Result<Option<&Self::Record>, AdapterError> {
        if &self.key == key {
            Ok(Some(&self.value))
        } else {
            Ok(None)
        }
    }

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.key == *key)
    }

    fn store_record(&mut self, key: Self::Key, record: Self::Record) -> Result<(), AdapterError> {
        self.key = key;
        self.value = record;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record_exists_works() {
        let value = "three".to_string();
        let adapter = DumbAdapter::new(3, Box::new(value));

        assert!(
            adapter.record_exists(&3).unwrap(),
            "checking an existing key returns true"
        );
        assert!(
            !adapter.record_exists(&0).unwrap(),
            "checking a non existing key returns false"
        );
    }

    #[test]
    fn get_record_works() {
        let value = "three".to_string();
        let adapter = DumbAdapter::new(3, Box::new(value));

        assert_eq!(
            "three",
            adapter.get_record(&3).unwrap().unwrap(),
            "getting a record from an existing key"
        );
        assert!(
            adapter.get_record(&0).unwrap().is_none(),
            "getting a record with a non existing key should return None"
        );
    }

    #[test]
    fn store_record_works() {
        let value = "three".to_string();
        let mut adapter = DumbAdapter::new(3, Box::new(value));

        assert!(
            adapter.store_record(0, "zero".to_string()).is_ok(),
            "storing a valid key should return OK"
        );
        assert_eq!(
            "zero",
            adapter.get_record(&0).unwrap().unwrap(),
            "using the same key the record should be the same"
        );
    }
}
