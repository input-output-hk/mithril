//! The module used for handling eras

pub mod adapters;
mod era_checker;
mod era_reader;
mod supported_era;

pub use adapters::EraReaderAdapterType;
pub use era_checker::EraChecker;
pub use era_reader::*;
pub use supported_era::SupportedEra;
