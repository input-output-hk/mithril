//! Tools to compute mithril digest from a Cardano node database.

pub mod cache;
mod cardano_immutable_digester;
mod dummy_immutable_db_builder;
mod immutable_digester;
mod immutable_file;
mod immutable_file_observer;

pub use cardano_immutable_digester::CardanoImmutableDigester;
pub use immutable_digester::{DumbImmutableDigester, ImmutableDigester, ImmutableDigesterError};
pub use immutable_file::{ImmutableFile, ImmutableFileCreationError, ImmutableFileListingError};
pub use immutable_file_observer::{
    DumbImmutableFileObserver, ImmutableFileObserver, ImmutableFileObserverError,
    ImmutableFileSystemObserver,
};

#[cfg(test)]
pub use dummy_immutable_db_builder::DummyImmutablesDbBuilder;
