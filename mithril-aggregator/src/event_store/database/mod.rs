//! database module.
//! This module contains the entities definition tied with database
//! representation with their associated queries.

pub mod migration;
pub(crate) mod query;
pub(crate) mod record;
mod repository;

pub use repository::*;
#[cfg(test)]
pub(crate) mod test_helper;
