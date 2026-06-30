mod fake_aggregator_http;
mod state_machine_tester;

pub use fake_aggregator_http::FakeAggregatorHttpServer;
pub use state_machine_tester::StateMachineTester;

use mithril_common::test::TempDir;
use slog::Drain;
use std::path::PathBuf;
use std::sync::Arc;

/// Create a directory to save test artifacts. This directory is cleaned if it
/// already exists, it is created if not. This directory is kept at the end to
/// allow debugging.
pub fn get_test_dir(subdir_name: &str) -> PathBuf {
    TempDir::create("signer-integration", subdir_name)
}

pub fn stdout_logger() -> slog::Logger {
    let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    slog::Logger::root(Arc::new(drain), slog::o!())
}
