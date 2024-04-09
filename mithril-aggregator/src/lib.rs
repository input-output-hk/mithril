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

mod artifact_builder;
mod commands;
mod configuration;
pub mod database;
pub mod dependency_injection;
pub mod entities;
pub mod event_store;
mod http_server;
mod message_adapters;
mod multi_signer;
mod runtime;
pub mod services;
mod signer_registerer;
mod snapshot_uploaders;
mod snapshotter;
mod store;
mod tools;

pub use crate::artifact_builder::ArtifactBuilder;
pub use crate::configuration::{
    Configuration, DefaultConfiguration, ExecutionEnvironment, SnapshotUploaderType,
    ZstandardCompressionParameters,
};
pub use crate::multi_signer::{MultiSigner, MultiSignerImpl};
pub use commands::{CommandType, MainOpts};
pub use dependency_injection::DependencyContainer;
pub use message_adapters::{
    FromRegisterSignerAdapter, ToCertificatePendingMessageAdapter, ToEpochSettingsMessageAdapter,
};
pub use runtime::{
    AggregatorConfig, AggregatorRunner, AggregatorRunnerTrait, AggregatorRuntime, RuntimeError,
};
pub use signer_registerer::{
    MithrilSignerRegisterer, SignerRecorder, SignerRegisterer, SignerRegistrationError,
    SignerRegistrationRound, SignerRegistrationRoundOpener,
};
pub use snapshot_uploaders::{
    DumbSnapshotUploader, LocalSnapshotUploader, RemoteSnapshotUploader, SnapshotUploader,
};
pub use snapshotter::{
    CompressedArchiveSnapshotter, DumbSnapshotter, SnapshotError, Snapshotter,
    SnapshotterCompressionAlgorithm,
};
pub use store::{
    CertificatePendingStore, ProtocolParametersStorer, VerificationKeyStore, VerificationKeyStorer,
};
pub use tools::{
    CExplorerSignerRetriever, SignersImporter, SignersImporterPersister, SignersImporterRetriever,
};

#[cfg(test)]
pub use dependency_injection::tests::initialize_dependencies;

#[cfg(test)]
pub(crate) mod test_tools {
    use slog::Drain;
    use std::sync::Arc;

    pub fn logger_for_tests() -> slog::Logger {
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }
}
