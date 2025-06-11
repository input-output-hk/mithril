//! The module used for handling eras

pub mod adapters;
mod era_checker;
mod era_reader;

pub use era_checker::EraChecker;
pub use era_reader::*;

/// Macro used to mark the code that should be cleaned up when the new era is activated
#[macro_export]
macro_rules! era_deprecate {
    ( $comment:literal ) => {};
}
