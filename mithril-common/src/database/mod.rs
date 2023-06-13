//! database module.
//! This module contains providers and entities shared between all application types.

mod db_version;
mod version_checker;

/// Database version.
pub type DbVersion = i64;

/// Format for dates in the database, ex:
///
/// Example: `2023-06-13 15:49:40.921132697` (nanoseconds are optionals)
pub const DB_DATE_FORMAT: &str = "%F %T%.f";

pub use db_version::*;
pub use version_checker::{DatabaseVersionChecker, SqlMigration};
