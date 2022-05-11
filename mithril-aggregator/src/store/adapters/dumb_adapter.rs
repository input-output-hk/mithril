use super::Adapter;
use std::io::Error as IOError;
use std::{error::Error, io::ErrorKind};

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

    fn get_record(&self, key: &Self::Key) -> Result<Option<&Self::Record>, Box<dyn Error>> {
        if &self.key == key {
            Ok(Some(&self.value))
        } else {
            Ok(None)
        }
    }

    fn record_exists(&self, key: &Self::Key) -> Result<bool, Box<dyn Error>> {
        Ok(self.key == *key)
    }

    fn store_record(
        &mut self,
        _key: Self::Key,
        _record: Self::Record,
    ) -> Result<(), Box<dyn Error>> {
        Err(Box::new(IOError::new(
            ErrorKind::AlreadyExists,
            "cannot store in dumb adapter",
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dumb_adapter_exists() {
        let value = "three".to_string();
        let adapter = DumbAdapter::new(3, Box::new(value));

        assert!(adapter.record_exists(&3).unwrap());
        assert!(!adapter.record_exists(&0).unwrap());
    }
}
