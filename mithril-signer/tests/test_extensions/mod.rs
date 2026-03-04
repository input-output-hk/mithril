mod fake_aggregator;
mod state_machine_tester;

pub use fake_aggregator::FakeAggregator;
pub use state_machine_tester::StateMachineTester;

use mithril_common::test::TempDir;
use std::path::PathBuf;

/// Create a directory to save test artifacts. This directory is cleaned if it
/// already exists, it is created if not. This directory is kept at the end to
/// allow debugging.
pub fn get_test_dir(subdir_name: &str) -> PathBuf {
    TempDir::create("signer-integration", subdir_name)
}
