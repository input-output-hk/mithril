//! Test utilities.
//!
//! ⚠ Do not use in production code ⚠
//!
//! This module provides in particular the [DummyCardanoDbBuilder] to generate and test doubles for the traits defined in this crate.

pub mod double;
mod dummy_cardano_db;
pub mod fake_data;

pub use dummy_cardano_db::{DummyCardanoDb, DummyCardanoDbBuilder};

#[cfg(test)]
pub(crate) struct TestLogger;

#[cfg(test)]
impl TestLogger {
    fn from_writer<W: std::io::Write + Send + 'static>(writer: W) -> slog::Logger {
        use slog::Drain;
        use std::sync::Arc;

        let decorator = slog_term::PlainDecorator::new(writer);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        slog::Logger::root(Arc::new(drain), slog::o!())
    }

    pub(crate) fn stdout() -> slog::Logger {
        Self::from_writer(slog_term::TestStdoutWriter)
    }
}
