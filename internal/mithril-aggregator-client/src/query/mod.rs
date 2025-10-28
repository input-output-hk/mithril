//! Provides queries to retrieve or send data to a Mithril aggregator
//!
//! Available queries
//! - Certificate: Get by hash, get latest genesis certificate
//!
mod api;
mod get;

pub(crate) use api::*;
pub use get::*;
