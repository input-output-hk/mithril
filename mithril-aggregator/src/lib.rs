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

mod certificate_creator;
mod command_args;
mod configuration;
pub mod database;
mod dependency;
pub mod dependency_injection;
pub mod event_store;
mod http_server;
mod message_adapters;
mod multi_signer;
mod runtime;
mod signer_registerer;
mod snapshot_stores;
mod snapshot_uploaders;
mod snapshotter;
pub mod stake_distribution_service;
mod store;
pub mod ticker_service;
mod tools;

pub use crate::configuration::{
    Configuration, DefaultConfiguration, ExecutionEnvironment, SnapshotUploaderType,
};
pub use crate::multi_signer::{MultiSigner, MultiSignerImpl, ProtocolError};
pub use crate::snapshot_stores::{LocalSnapshotStore, SnapshotStore};
pub use certificate_creator::{CertificateCreator, MithrilCertificateCreator};
pub use command_args::MainOpts;
pub use dependency::DependencyManager;
pub use message_adapters::{
    FromRegisterSignerAdapter, ToCertificatePendingMessageAdapter, ToEpochSettingsMessageAdapter,
};
pub use runtime::{
    AggregatorConfig, AggregatorRunner, AggregatorRunnerTrait, AggregatorRuntime, RuntimeError,
};
pub use signer_registerer::{
    MithrilSignerRegisterer, SignerRegisterer, SignerRegistrationError, SignerRegistrationRound,
    SignerRegistrationRoundOpener,
};
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
