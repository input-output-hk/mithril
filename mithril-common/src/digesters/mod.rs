//! Tools to compute mithril digest from a Cardano node database.

pub mod cache;
mod cardano_immutable_digester;
mod dumb_immutable_digester;
mod immutable_digester;
mod immutable_file;
mod immutable_file_observer;

pub use cardano_immutable_digester::CardanoImmutableDigester;
pub use immutable_digester::{ImmutableDigester, ImmutableDigesterError};
pub use immutable_file::{ImmutableFile, ImmutableFileCreationError, ImmutableFileListingError};
pub use immutable_file_observer::{
    DumbImmutableFileObserver, ImmutableFileObserver, ImmutableFileObserverError,
    ImmutableFileSystemObserver,
};

pub use dumb_immutable_digester::DumbImmutableDigester;

cfg_test_tools! {
    mod dummy_immutable_db_builder;

    pub use dummy_immutable_db_builder::{DummyImmutableDb, DummyImmutablesDbBuilder};
}
