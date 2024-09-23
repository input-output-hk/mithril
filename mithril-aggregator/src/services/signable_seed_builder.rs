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
    use mithril_common::{entities::Epoch, test_utils::MithrilFixtureBuilder};

    use crate::services::FakeEpochService;

    use super::*;

    #[tokio::test]
    async fn test_compute_seed_protocol_message() {
        let epoch = Epoch(5);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let expected_next_aggregate_verification_key = next_fixture.compute_and_encode_avk();
        let epoch_service = Arc::new(RwLock::new(FakeEpochService::with_data(
            epoch,
            &fixture.protocol_parameters(),
            &next_fixture.protocol_parameters(),
            &next_fixture.protocol_parameters(),
            &fixture.signers_with_stake(),
            &next_fixture.signers_with_stake(),
        )));
        let signable_seed_builder_service = SignableSeedBuilderService::new(epoch_service);

        let protocol_message = signable_seed_builder_service
            .compute_seed_protocol_message()
            .await
            .unwrap();

        assert_eq!(
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey),
            Some(&expected_next_aggregate_verification_key)
        );
    }
}
