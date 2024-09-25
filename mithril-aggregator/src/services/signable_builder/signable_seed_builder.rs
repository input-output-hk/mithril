//! ## AggregatorSignableSeedBuilder
//!
//! This service is responsible for computing the seed protocol message
//! that is used by the [SignableBuilder] to compute the final protocol message.
//!
use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{
    entities::ProtocolMessagePartValue, signable_builder::SignableSeedBuilder, StdResult,
};

use crate::services::EpochService;

/// SignableSeedBuilder aggregator implementation
pub struct AggregatorSignableSeedBuilder {
    epoch_service: Arc<RwLock<dyn EpochService>>,
}

impl AggregatorSignableSeedBuilder {
    /// AggregatorSignableSeedBuilder factory
    pub fn new(epoch_service: Arc<RwLock<dyn EpochService>>) -> Self {
        Self { epoch_service }
    }
}

#[async_trait]
impl SignableSeedBuilder for AggregatorSignableSeedBuilder {
    async fn compute_next_aggregate_verification_key_protocol_message_part_value(
        &self,
    ) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let next_aggregate_verification_key = (*epoch_service)
            .next_aggregate_verification_key()?
            .to_json_hex()
            .with_context(|| "convert next avk to json hex failure")?
            .to_string();

        Ok(next_aggregate_verification_key)
    }

    /// Compute next protocol parameters protocol message part value
    async fn compute_next_protocol_parameters_protocol_message_part_value(
        &self,
    ) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let next_protocol_parameters = epoch_service.next_protocol_parameters()?.compute_hash();

        Ok(next_protocol_parameters)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Epoch, test_utils::MithrilFixtureBuilder};

    use crate::services::FakeEpochService;

    use super::*;

    #[tokio::test]
    async fn test_compute_next_aggregate_verification_key_protocol_message_value() {
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
        let signable_seed_builder = AggregatorSignableSeedBuilder::new(epoch_service);

        let next_aggregate_verification_key = signable_seed_builder
            .compute_next_aggregate_verification_key_protocol_message_part_value()
            .await
            .unwrap();

        assert_eq!(
            next_aggregate_verification_key,
            expected_next_aggregate_verification_key
        );
    }

    #[tokio::test]
    async fn test_compute_next_protocol_parameters_protocol_message_value() {
        let epoch = Epoch(5);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let expected_next_protocol_parameters = next_fixture.protocol_parameters().compute_hash();
        let epoch_service = Arc::new(RwLock::new(FakeEpochService::with_data(
            epoch,
            &fixture.protocol_parameters(),
            &next_fixture.protocol_parameters(),
            &next_fixture.protocol_parameters(),
            &fixture.signers_with_stake(),
            &next_fixture.signers_with_stake(),
        )));
        let signable_seed_builder = AggregatorSignableSeedBuilder::new(epoch_service);

        let next_protocol_parameters = signable_seed_builder
            .compute_next_protocol_parameters_protocol_message_part_value()
            .await
            .unwrap();

        assert_eq!(next_protocol_parameters, expected_next_protocol_parameters);
    }
}
