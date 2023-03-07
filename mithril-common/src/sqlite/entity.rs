use sqlite::Row;
use thiserror::Error;

use super::Projection;

/// SqLite hydration error
#[derive(Error, Debug)]
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
    /// This method is intended to be used when creating new instances from SQL
    /// result rows. This is the place to grab data, check consistency and types
    /// and return the entity if possible.
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized;

    /// Construct a [Projection] that will allow to hydrate this `SqLiteEntity`.
    fn get_projection() -> Projection;
}
