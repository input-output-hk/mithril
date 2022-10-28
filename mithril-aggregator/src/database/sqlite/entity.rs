use sqlite::Row;
use thiserror::Error;

/// SqLite hydration error
#[derive(Error, Debug, Clone)]
pub enum HydrationError {
    /// data do not conform to expectations
    #[error("data do not conform to expectations: {0}")]
    InvalidData(String),

    /// data are missing
    #[error("some data are missing: {0}")]
    MissingData(String),

    /// inconsistent data
    #[error("data type inconsistency: {0}")]
    InconsistentType(String),
}

/// How to hydrate an entity from a SQLite result row
pub trait SqLiteEntity {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized;
}
