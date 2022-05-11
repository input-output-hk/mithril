use super::AdapterError;

pub trait Adapter {
    type Key;
    type Record;

    fn store_record(&mut self, key: Self::Key, record: Self::Record) -> Result<(), AdapterError>;

    fn get_record(&self, key: &Self::Key) -> Result<Option<&Self::Record>, AdapterError>;

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError>;
}
