//! Test utilities
//!
//! They contains:
//! * A Open Api Spec tester
//! * Some precomputed fake data and keys
//! * A builder of [MithrilFixture] to generate signers alongside a stake distribution
//!

#[cfg(feature = "apispec")]
#[cfg_attr(docsrs, doc(cfg(feature = "apispec")))]
pub mod apispec;

pub mod fake_data;
pub mod fake_keys;

mod fixture_builder;
mod mithril_fixture;

mod temp_dir;

#[cfg(feature = "test_http_server")]
#[cfg_attr(docsrs, doc(cfg(feature = "test_http_server")))]
pub mod test_http_server;

pub use fixture_builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
pub use mithril_fixture::{MithrilFixture, SignerFixture};
pub use temp_dir::*;

#[cfg(test)]
pub(crate) fn logger_for_tests() -> slog::Logger {
    use slog::Drain;
    use std::sync::Arc;

    let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    slog::Logger::root(Arc::new(drain), slog::o!())
}
