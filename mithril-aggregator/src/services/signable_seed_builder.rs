//! ## SignableSeedBuilderService
//!
//! This service is responsible for computing the seed protocol message
//! that is used by the [SignableBuilder] to compute the final protocol message.
//!
use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{
    entities::{ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableSeedBuilder,
    StdResult,
};

use crate::services::EpochService;

/// SignableSeedBuilder service
pub struct SignableSeedBuilderService {
    epoch_service: Arc<RwLock<dyn EpochService>>,
}

impl SignableSeedBuilderService {
    /// SignableSeedBuilderService factory
    pub fn new(epoch_service: Arc<RwLock<dyn EpochService>>) -> Self {
        Self { epoch_service }
    }
}

#[async_trait]
impl SignableSeedBuilder for SignableSeedBuilderService {
    /// Compute seed protocol message
    async fn compute_seed_protocol_message(&self) -> StdResult<ProtocolMessage> {
        let epoch_service = self.epoch_service.read().await;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            (*epoch_service)
                .next_aggregate_verification_key()?
                .to_json_hex()
                .with_context(|| "convert next avk to json hex failure")?
                .to_string(),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_compute_seed_protocol_message() {
        // TODO: implement test
    }
}
