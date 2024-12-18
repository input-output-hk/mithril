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
    mod dummy_cardano_db;

    pub use dummy_cardano_db::{DummyCardanoDb, DummyCardanoDbBuilder, IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR};
}
