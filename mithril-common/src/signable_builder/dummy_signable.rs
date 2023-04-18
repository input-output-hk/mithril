use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use crate::{
    entities::{Epoch, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::{Beacon, Signable, SignableBuilder},
    StdResult,
};

/// Dummy beacon
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct DummyBeacon {
    /// Epoch of the beacon
    pub epoch: Epoch,
}

impl Beacon for DummyBeacon {}

/// Dummy signable
pub struct DummySignable {
    message: String,
    beacon: DummyBeacon,
}

impl DummySignable {
    /// Dummy signable factory
    pub fn new(message: String, beacon: DummyBeacon) -> Self {
        Self { message, beacon }
    }
}

impl Signable for DummySignable {
    fn compute_protocol_message(&self) -> StdResult<ProtocolMessage> {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            format!("message={}, epoch={}", self.message, self.beacon.epoch),
        );

        Ok(protocol_message)
    }
}

/// A [DummySignable] builder
pub struct DummySignableBuilder {}

impl DummySignableBuilder {
    /// Dummy signable builder factory
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for DummySignableBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl SignableBuilder<DummyBeacon, DummySignable> for DummySignableBuilder {
    async fn compute_signable(&self, beacon: DummyBeacon) -> StdResult<DummySignable> {
        Ok(DummySignable::new("computed_message".to_string(), beacon))
    }
}
