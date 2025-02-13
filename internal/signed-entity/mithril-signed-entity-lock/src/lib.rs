#![warn(missing_docs)]

//! This module provides a non-blocking lock mechanism for signed entity types to prevent multiple
//! modification on a same entity type at the same time.

mod signed_entity_type_lock;

pub use signed_entity_type_lock::*;
