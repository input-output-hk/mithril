#![warn(missing_docs)]
//! This crate provides mechanisms to publish and consume messages of a Decentralized Message Queue network through a DMQ node.

mod consumer;
mod message;
mod publisher;
pub mod test;

use std::ops::{Deref, DerefMut};

pub use consumer::{DmqConsumer, DmqConsumerPallas, DmqConsumerServer, DmqConsumerServerPallas};
pub use message::DmqMessageBuilder;
pub use publisher::{
    DmqPublisher, DmqPublisherPallas, DmqPublisherServer, DmqPublisherServerPallas,
};

// Re-export the DMQ message type for convenience.
pub use pallas_network::miniprotocols::localmsgsubmission::DmqMsg;

// TODO: move to own sub-module
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Type alias for a DMQ message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DmqMessage(DmqMsg);

#[derive(Serialize, Deserialize)]
struct RawBytes(#[serde(with = "serde_bytes")] Vec<u8>);

impl Deref for DmqMessage {
    type Target = DmqMsg;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for DmqMessage {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<DmqMsg> for DmqMessage {
    fn from(msg: DmqMsg) -> Self {
        Self(msg)
    }
}

impl From<DmqMessage> for DmqMsg {
    fn from(msg: DmqMessage) -> Self {
        msg.0
    }
}

impl Serialize for DmqMessage {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let raw_bytes = RawBytes(
            bincode::serde::encode_to_vec(self, bincode::config::standard()).map_err(|e| {
                serde::ser::Error::custom(format!("DMQ message serialization error: {e}"))
            })?,
        );

        raw_bytes.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for DmqMessage {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let raw_bytes = RawBytes::deserialize(deserializer)?;
        let (res, _) =
            bincode::serde::decode_from_slice::<Self, _>(&raw_bytes.0, bincode::config::standard())
                .map_err(|e| {
                    serde::de::Error::custom(format!("DMQ message deserialization error: {e}"))
                })?;

        Ok(res)
    }
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
