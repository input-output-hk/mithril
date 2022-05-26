use thiserror::Error;

#[derive(Debug, Error)]
pub enum AdapterError {
    #[error("something wrong happened: {0}")]
    GeneralError(String),
    #[error("problem creating the repository: {0}")]
    InitializationError(Box<dyn std::error::Error>),
    #[error("problem opening the IO stream: {0}")]
    OpeningStreamError(Box<dyn std::error::Error>),
    #[error("problem parsing the IO stream: {0}")]
    ParsingDataError(Box<dyn std::error::Error>),
}
pub trait StoreAdapter {
    type Key;
    type Record;

    fn store_record(&mut self, key: &Self::Key, record: &Self::Record) -> Result<(), AdapterError>;

    fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError>;

    fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError>;

    fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError>;
}
