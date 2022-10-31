//! SQLite module.
//! This module provides a minimal yet useful Entity framework on top of SQLite
//! with ability to perform any SQL query possible and hydrate results in Rust
//! structs.
mod cursor;
mod entity;
mod projection;
mod provider;

pub use cursor::EntityCursor;
pub use entity::{HydrationError, SqLiteEntity};
pub use projection::{Projection, ProjectionField};
pub use provider::Provider;
