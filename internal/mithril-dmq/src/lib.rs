#![warn(missing_docs)]
//! This crate provides mechanisms to publish and consume messages of a Decentralized Message Queue network through a DMQ node.

mod consumer;
mod model;
mod publisher;
pub mod test;

pub use consumer::{DmqConsumerClient, DmqConsumerClientPallas};
pub use model::{DmqMessage, DmqMessageBuilder};
pub use publisher::{DmqPublisherClient, DmqPublisherClientPallas};

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
