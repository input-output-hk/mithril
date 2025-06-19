#![warn(missing_docs)]
//! This crate provides components to read the files of a Cardano node internal database, such as
//! immutable files and ledger state snapshots, and compute digests from them.
//! It includes observers, digesters and various helpers for file system operations.
//!

pub mod digesters;
pub mod entities;
mod immutable_file_observer;
pub mod signable_builder;
pub mod test;

pub use immutable_file_observer::*;

/// Directory name for the immutable files.
pub const IMMUTABLE_DIR: &str = "immutable";
/// Directory name for the ledger files.
pub const LEDGER_DIR: &str = "ledger";
/// Directory name for the volatile files.
pub const VOLATILE_DIR: &str = "volatile";

/// Returns the names of the files that compose an immutable trio.
pub fn immutable_trio_names(
    immutable_file_number: mithril_common::entities::ImmutableFileNumber,
) -> Vec<String> {
    vec![
        format!("{:05}.chunk", immutable_file_number),
        format!("{:05}.primary", immutable_file_number),
        format!("{:05}.secondary", immutable_file_number),
    ]
}
