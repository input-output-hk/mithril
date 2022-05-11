use std::error::Error;

pub trait Adapter {
    type Key;
    type Record;

    fn store_record(&mut self, key: Self::Key, record: Self::Record) -> Result<(), Box<dyn Error>>;

    fn get_record(&self, key: &Self::Key) -> Result<Option<&Self::Record>, Box<dyn Error>>;

    fn record_exists(&self, key: &Self::Key) -> Result<bool, Box<dyn Error>>;
}
