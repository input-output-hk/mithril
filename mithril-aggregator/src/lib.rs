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
mod file_uploaders;
mod http_server;
mod message_adapters;
pub mod metrics;
mod multi_signer;
mod runtime;
pub mod services;
mod signer_registerer;
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
pub use file_uploaders::{
    DumbFileUploader, FileUploader, LocalFileUploader, RemoteSnapshotUploader,
};
pub use message_adapters::{FromRegisterSignerAdapter, ToCertificatePendingMessageAdapter};
pub use metrics::*;
pub use runtime::{
    AggregatorConfig, AggregatorRunner, AggregatorRunnerTrait, AggregatorRuntime, RuntimeError,
};
pub use signer_registerer::{
    MithrilSignerRegisterer, SignerRecorder, SignerRegisterer, SignerRegistrationError,
    SignerRegistrationRound, SignerRegistrationRoundOpener,
};
pub use snapshotter::{
    CompressedArchiveSnapshotter, DumbSnapshotter, SnapshotError, Snapshotter,
    SnapshotterCompressionAlgorithm,
};
pub use store::{CertificatePendingStorer, EpochSettingsStorer, VerificationKeyStorer};
pub use tools::{
    CExplorerSignerRetriever, SignersImporter, SignersImporterPersister, SignersImporterRetriever,
    SingleSignatureAuthenticator,
};

#[cfg(test)]
pub(crate) use dependency_injection::tests::initialize_dependencies;

// Memory allocator (to handle properly memory fragmentation)
#[cfg(all(not(target_env = "msvc"), feature = "jemallocator"))]
use tikv_jemallocator::Jemalloc;

#[cfg(all(not(target_env = "msvc"), feature = "jemallocator"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[cfg(test)]
pub(crate) mod test_tools {
    use std::fs::File;
    use std::io;
    use std::sync::Arc;

    use slog::{Drain, Logger};
    use slog_async::Async;
    use slog_term::{CompactFormat, PlainDecorator};

    pub struct TestLogger;

    impl TestLogger {
        fn from_writer<W: io::Write + Send + 'static>(writer: W) -> Logger {
            let decorator = PlainDecorator::new(writer);
            let drain = CompactFormat::new(decorator).build().fuse();
            let drain = Async::new(drain).build().fuse();
            Logger::root(Arc::new(drain), slog::o!())
        }

        pub fn stdout() -> Logger {
            Self::from_writer(slog_term::TestStdoutWriter)
        }

        pub fn file(filepath: &std::path::Path) -> Logger {
            Self::from_writer(File::create(filepath).unwrap())
        }
    }
}
