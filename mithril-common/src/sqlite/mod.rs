//! SQLite module.
//! This module provides a minimal yet useful Entity framework on top of SQLite
//! with ability to perform any SQL query possible and hydrate results in Rust
//! structs.
mod condition;
mod cursor;
mod entity;
mod projection;
mod provider;
mod source_alias;

pub use condition::WhereCondition;
pub use cursor::EntityCursor;
pub use entity::{HydrationError, SqLiteEntity};
pub use projection::{Projection, ProjectionField};
pub use provider::Provider;
pub use source_alias::SourceAlias;
