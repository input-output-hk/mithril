//! Test utilities.
//!
//! ⚠ Do not use in production code ⚠
//!
//! This module provides in particular the [DummyCardanoDbBuilder] to generate and test doubles for the traits defined in this crate.

pub mod double;
mod dummy_cardano_db;
pub mod fake_data;

pub use dummy_cardano_db::{DummyCardanoDb, DummyCardanoDbBuilder};

#[cfg(test)]
mithril_common::define_test_logger!();
