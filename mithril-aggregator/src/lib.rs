#![warn(missing_docs)]
//! Mithril aggregator
//! The Aggregator is responsible for:
//! * registering signers
//! * producing aggregate multi-signatures
//! * creating, storing & serving the certificate chain
//! * creating, storing & serving certified snapshots
//!
//! This crate is divided in two parts: a HTTP server that exposes an API to
//! communicate with signers and a Runtime that tracks the blockchain to provide
//! signed certificates.
//! You can find more information on how it works reading the [documentation website](https://mithril.network/doc/mithril/mithril-network/aggregator).

mod command_args;
mod configuration;
mod database;
mod dependency;
mod http_server;
mod multi_signer;
mod runtime;
mod snapshot_stores;
mod snapshot_uploaders;
mod snapshotter;
mod store;
mod tools;

const DATABASE_SCHEMATIC_VERSION: &str = "0.1.0";

pub use crate::configuration::{
    Configuration, DefaultConfiguration, GenesisConfiguration, SnapshotStoreType,
    SnapshotUploaderType,
};
pub use crate::multi_signer::{MultiSigner, MultiSignerImpl, ProtocolError};
pub use crate::snapshot_stores::{LocalSnapshotStore, RemoteSnapshotStore, SnapshotStore};
pub use command_args::MainOpts;
pub use database::{DatabaseVersion, VersionProvider, VersionUpdatedProvider};
pub use dependency::DependencyManager;
pub use http_server::Server;
pub use runtime::{AggregatorConfig, AggregatorRunner, AggregatorRunnerTrait, AggregatorRuntime};
pub use snapshot_uploaders::{
    DumbSnapshotUploader, LocalSnapshotUploader, RemoteSnapshotUploader, SnapshotUploader,
};
pub use snapshotter::{DumbSnapshotter, GzipSnapshotter, SnapshotError, Snapshotter};
pub use store::{
    CertificatePendingStore, CertificateStore, ProtocolParametersStore, ProtocolParametersStorer,
    SingleSignatureStore, VerificationKeyStore, VerificationKeyStorer,
};

#[cfg(test)]
pub use dependency::tests::initialize_dependencies;
