#![warn(missing_docs)]
#![doc = include_str!("../README.md")]

mod commands;
/// Peer to peer module
pub mod p2p;
mod relay;
mod repeater;

pub use commands::Args;
pub use commands::RelayCommands;
pub use relay::AggregatorRelay;
pub use relay::PassiveRelay;
pub use relay::SignerRelay;
pub use relay::SignerRelayMode;

/// The P2P topic names used by Mithril
pub mod mithril_p2p_topic {
    /// The topic name where HTTP signer registrations are published
    pub const SIGNERS_HTTP: &str = "mithril/signers/http";

    /// The topic name where HTTP signatures are published
    pub const SIGNATURES_HTTP: &str = "mithril/signatures/http";

    /// The topic name where DMQ signatures are published
    pub const SIGNATURES_DMQ: &str = "mithril/signatures/dmq";
}

#[cfg(test)]
pub(crate) mod test_tools {
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
    }
}
