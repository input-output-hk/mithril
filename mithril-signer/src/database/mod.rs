//! database module.
//! This module contains the entities definition tied with database
//! representation with their associated providers.
pub mod migration;
pub mod repository;
#[cfg(test)]
pub(crate) mod test_helper;
