//! Provides queries to retrieve or send data to a Mithril aggregator
//!
//! Available queries
//! - Certificate: Get by hash
//!
mod api;
mod certificate;

pub(crate) use api::*;
pub use certificate::*;
