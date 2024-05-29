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

mod cardano_transactions_builder;
mod fixture_builder;
mod mithril_fixture;

mod temp_dir;

#[cfg(feature = "test_http_server")]
#[cfg_attr(docsrs, doc(cfg(feature = "test_http_server")))]
pub mod test_http_server;

pub use cardano_transactions_builder::CardanoTransactionsBuilder;
pub use fixture_builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
pub use mithril_fixture::{MithrilFixture, SignerFixture};
pub use temp_dir::*;
#[cfg(test)]
pub use utils::*;

#[cfg(test)]
mod utils {
    use std::fs::File;
    use std::io;
    use std::sync::Arc;

    use slog::{Drain, Logger};
    use slog_async::Async;
    use slog_term::{CompactFormat, PlainDecorator};

    pub struct TestLogger;

    #[cfg(test)]
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
