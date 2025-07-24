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
mod immutable_file_digest_mapper;
mod message_adapters;
pub mod metrics;
mod multi_signer;
mod runtime;
pub mod services;
mod store;
#[doc(hidden)]
pub mod test;
mod tools;

pub use crate::artifact_builder::ArtifactBuilder;
pub use crate::configuration::{
    ConfigurationSource, DefaultConfiguration, ExecutionEnvironment, ServeCommandConfiguration,
    SnapshotUploaderType, ZstandardCompressionParameters,
};
pub use crate::multi_signer::{MultiSigner, MultiSignerImpl};
pub use commands::{CommandType, MainOpts};
pub use dependency_injection::ServeCommandDependenciesContainer;
pub use file_uploaders::{DumbUploader, FileUploader};
pub use message_adapters::FromRegisterSignerAdapter;
pub use metrics::*;
pub use runtime::{
    AggregatorConfig, AggregatorRunner, AggregatorRunnerTrait, AggregatorRuntime, RuntimeError,
};
pub use services::{
    MithrilSignerRegistrationFollower, MithrilSignerRegistrationLeader,
    MithrilSignerRegistrationVerifier, SignerRecorder, SignerRegisterer, SignerRegistrationError,
    SignerRegistrationRound, SignerRegistrationRoundOpener, SignerRegistrationVerifier,
    SignerSynchronizer,
};
pub use store::{EpochSettingsStorer, ProtocolParametersRetriever, VerificationKeyStorer};
pub use tools::{
    CExplorerSignerRetriever, SignersImporter, SignersImporterPersister, SignersImporterRetriever,
    SingleSignatureAuthenticator,
};

pub use immutable_file_digest_mapper::ImmutableFileDigestMapper;

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
    use std::io;
    use std::sync::Arc;

    use slog::{Drain, Logger};
    use slog_async::Async;
    use slog_term::{CompactFormat, PlainDecorator};

    use mithril_common::test::{MemoryDrainForTest, MemoryDrainForTestInspector};

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

        pub fn memory() -> (Logger, MemoryDrainForTestInspector) {
            let (drain, inspector) = MemoryDrainForTest::new();
            (Logger::root(drain.fuse(), slog::o!()), inspector)
        }
    }
}
