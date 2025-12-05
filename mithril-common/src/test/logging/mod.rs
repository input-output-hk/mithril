//! Logging utilities for tests
//!
mod memory_logger;

pub use memory_logger::*;

/// Creates a test-specific `TestLogger` struct that can creates preconfigured logger instances.
///
/// This macro avoids direct `slog_async` and `slog_term` dependencies in library crates
/// by letting dependents add them as `dev-dependencies` only.
///
/// ## Methods
///
/// - `TestLogger::stdout()` - Logger that outputs to stdout.
/// - `TestLogger::memory()` - Logger that stores messages in memory for inspection and still outputs stdout.
///   Returns `(Logger, MemoryDrainForTestInspector)` tuple
///
/// Requires: `slog`, `slog_async`, `slog_term`
#[macro_export]
macro_rules! define_test_logger {
    () => {
        /// Logger builder for tests
        #[cfg(test)]
        pub(crate) struct TestLogger;

        #[cfg(test)]
        mod test_logger_impl {
            use std::io;

            use slog::{Drain, Logger};
            use slog_async::Async;
            use slog_term::{CompactFormat, PlainDecorator, TestStdoutWriter};

            use $crate::test::logging::{MemoryDrainForTest, MemoryDrainForTestInspector};

            impl super::TestLogger {
                fn from_writer<W: io::Write + Send + 'static>(writer: W) -> Async {
                    let decorator = PlainDecorator::new(writer);
                    let drain = CompactFormat::new(decorator).build().fuse();
                    Async::new(drain).build()
                }

                /// Create a logger that outputs to stdout.
                pub(crate) fn stdout() -> Logger {
                    Logger::root(Self::from_writer(TestStdoutWriter).fuse(), slog::o!())
                }

                /// Create a logger that stores messages in memory for inspection and still outputs stdout.
                pub(crate) fn memory() -> (Logger, MemoryDrainForTestInspector) {
                    let (memory_drain, inspector) = MemoryDrainForTest::new();
                    (Logger::root(memory_drain.fuse(), slog::o!()), inspector)
                }
            }
        }
    };
}
pub use define_test_logger;
