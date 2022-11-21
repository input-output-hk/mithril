//! database module.
//! This module contains providers and entities shared between all application types.

mod db_version;
mod version_checker;

pub use db_version::*;
pub use version_checker::{ApplicationVersionChecker, SqlMigration};
