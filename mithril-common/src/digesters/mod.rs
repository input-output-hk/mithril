//! Tools to compute mithril digest from a Cardano node database.

pub mod cache;
mod cardano_immutable_digester;
mod dumb_immutable_digester;
mod immutable_digester;
mod immutable_file;
mod immutable_file_observer;

pub use cardano_immutable_digester::CardanoImmutableDigester;
pub use immutable_digester::{
    ComputedImmutablesDigests, ImmutableDigester, ImmutableDigesterError,
};
pub use immutable_file::{ImmutableFile, ImmutableFileCreationError, ImmutableFileListingError};
pub use immutable_file_observer::{
    DumbImmutableFileObserver, ImmutableFileObserver, ImmutableFileObserverError,
    ImmutableFileSystemObserver,
};

pub use dumb_immutable_digester::DumbImmutableDigester;

/// Directory name for the immutable files.
pub const IMMUTABLE_DIR: &str = "immutable";
/// Directory name for the ledger files.
pub const LEDGER_DIR: &str = "ledger";
/// Directory name for the volatile files.
pub const VOLATILE_DIR: &str = "volatile";

/// Returns the names of the files that compose an immutable trio.
pub fn immutable_trio_names(
    immutable_file_number: crate::entities::ImmutableFileNumber,
) -> Vec<String> {
    vec![
        format!("{:05}.chunk", immutable_file_number),
        format!("{:05}.primary", immutable_file_number),
        format!("{:05}.secondary", immutable_file_number),
    ]
}

cfg_test_tools! {
    mod dummy_cardano_db;

    pub use dummy_cardano_db::{DummyCardanoDb, DummyCardanoDbBuilder};
}
