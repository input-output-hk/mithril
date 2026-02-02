//! DmqMessageTestPayload module for tests only

use std::fmt::Debug;

use mithril_common::{
    StdResult,
    crypto_helper::{TryFromBytes, TryToBytes},
    test::double::Dummy,
};

/// A test message payload for the DMQ.
#[derive(PartialEq, Eq, Clone, Hash)]
pub struct DmqMessageTestPayload {
    message: Vec<u8>,
}

impl DmqMessageTestPayload {
    /// Creates a new `DmqMessageTestPayload` with the given bytes.
    pub fn new(bytes: &[u8]) -> Self {
        Self {
            message: bytes.to_vec(),
        }
    }
}

impl Dummy for DmqMessageTestPayload {
    /// Creates a dummy `DmqMessageTestPayload` with a predefined message.
    fn dummy() -> Self {
        Self {
            message: b"dummy message".to_vec(),
        }
    }
}

impl Debug for DmqMessageTestPayload {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DmqMessageTestPayload")
            .field("message", &self.message)
            .finish()
    }
}

impl TryToBytes for DmqMessageTestPayload {
    fn to_bytes_vec(&self) -> StdResult<Vec<u8>> {
        Ok(self.message.clone())
    }
}

impl TryFromBytes for DmqMessageTestPayload {
    fn try_from_bytes(bytes: &[u8]) -> StdResult<Self> {
        Ok(Self {
            message: bytes.to_vec(),
        })
    }
}
