//! database module.
//! This module contains providers and entities shared between all application types.

mod db_version;
mod signed_entity_hydrator;
mod version_checker;

/// Database version.
pub type DbVersion = i64;

pub use db_version::*;
pub use signed_entity_hydrator::SignedEntityTypeHydrator;
pub use version_checker::{DatabaseVersionChecker, SqlMigration};
