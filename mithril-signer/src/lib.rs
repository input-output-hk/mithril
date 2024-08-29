#![warn(missing_docs)]
//! Mithril Signer crate documentation
//!
//! This crate is used by Cardano nodes to participate to Mithril signatures.
//! It proposes tools to communicate with Mithril aggregators and to issue Single Signatures.
//! See the [Mithril documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-signer)
//! for more information on how it works.

mod configuration;
pub mod database;
pub mod dependency_injection;
mod message_adapters;
pub mod metrics;
mod runtime;
pub mod services;
pub mod store;

pub use configuration::{Configuration, DefaultConfiguration};
pub use message_adapters::{
    FromEpochSettingsAdapter, FromPendingCertificateMessageAdapter, ToRegisterSignerMessageAdapter,
};
pub use metrics::*;
pub use runtime::*;

/// HTTP request timeout duration in milliseconds
const HTTP_REQUEST_TIMEOUT_DURATION: u64 = 30000;

/// SQLite file names
const SQLITE_FILE: &str = "signer.sqlite3";
const SQLITE_FILE_CARDANO_TRANSACTION: &str = "cardano-transaction.sqlite3";

// Memory allocator (to handle properly memory fragmentation)
#[cfg(all(not(target_env = "msvc"), feature = "jemallocator"))]
use tikv_jemallocator::Jemalloc;

#[cfg(all(not(target_env = "msvc"), feature = "jemallocator"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

#[cfg(test)]
pub mod test_tools {
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
