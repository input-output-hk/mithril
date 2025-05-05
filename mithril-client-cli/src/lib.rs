#![warn(missing_docs)]

//! A command line interface that uses the [Mithril Client Library](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-library)
//! to manipulate Mithril certified types from a Mithril Aggregator:
//! - Cardano Database v1 (aka Snapshot): list, show, download archive
//! - Cardano Database v2: list, show, download archive
//! - Cardano transactions: list & show snapshot, certify a list of transactions
//! - Cardano stake distribution: list and download
//! - Mithril stake distribution: list and download
//!
//!   You can find more information on how it works reading the [documentation website](https://mithril.network/doc/mithril/mithril-network/client).

mod command_context;
pub mod commands;
mod configuration;
mod utils;

pub use command_context::*;
/// Error Clap
pub type ClapError = clap::error::Error;

#[cfg(test)]
pub(crate) mod test_utils {
    use std::io;
    use std::sync::Arc;

    use slog::{Drain, Logger};
    use slog_async::Async;
    use slog_term::{CompactFormat, PlainDecorator};

    use mithril_common::test_utils::{MemoryDrainForTest, MemoryDrainForTestInspector};

    pub struct TestLogger;

    #[allow(unused)]
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
