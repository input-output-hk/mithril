//! The module used for handling eras

pub mod adapters;
mod era_checker;
mod era_reader;

pub use era_checker::EraChecker;
pub use era_reader::*;
