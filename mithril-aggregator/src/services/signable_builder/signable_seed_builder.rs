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
    async fn compute_next_aggregate_verification_key(&self) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let next_aggregate_verification_key = (*epoch_service)
            .next_aggregate_verification_key()?
            .to_json_hex()
            .with_context(|| "convert next avk to json hex failure")?
            .to_string();

        Ok(next_aggregate_verification_key)
    }

    async fn compute_next_protocol_parameters(&self) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let next_protocol_parameters = epoch_service.next_protocol_parameters()?.compute_hash();

        Ok(next_protocol_parameters)
    }

    async fn compute_current_epoch(&self) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let current_epoch = epoch_service.epoch_of_current_data()?.to_string();

        Ok(current_epoch)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{Epoch, SignedEntityConfig},
        test_utils::{MithrilFixture, MithrilFixtureBuilder},
    };

    use crate::{entities::AggregatorEpochSettings, services::FakeEpochService};

    use super::*;

    fn build_signable_builder_service(
        epoch: Epoch,
        fixture: &MithrilFixture,
        next_fixture: &MithrilFixture,
    ) -> AggregatorSignableSeedBuilder {
        let epoch_service = Arc::new(RwLock::new(FakeEpochService::with_data(
            epoch,
            &AggregatorEpochSettings {
                protocol_parameters: fixture.protocol_parameters(),
                ..AggregatorEpochSettings::dummy()
            },
            &AggregatorEpochSettings {
                protocol_parameters: next_fixture.protocol_parameters(),
                ..AggregatorEpochSettings::dummy()
            },
            &AggregatorEpochSettings {
                protocol_parameters: next_fixture.protocol_parameters(),
                ..AggregatorEpochSettings::dummy()
            },
            &fixture.signers_with_stake(),
            &next_fixture.signers_with_stake(),
            SignedEntityConfig::dummy(),
        )));

        AggregatorSignableSeedBuilder::new(epoch_service)
    }

    #[tokio::test]
    async fn test_compute_next_aggregate_verification_key_protocol_message_value() {
        let epoch = Epoch(5);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let signable_seed_builder = build_signable_builder_service(epoch, &fixture, &next_fixture);
        let expected_next_aggregate_verification_key = next_fixture.compute_and_encode_avk();

        let next_aggregate_verification_key = signable_seed_builder
            .compute_next_aggregate_verification_key()
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
        let signable_seed_builder = build_signable_builder_service(epoch, &fixture, &next_fixture);
        let expected_next_protocol_parameters = next_fixture.protocol_parameters().compute_hash();

        let next_protocol_parameters = signable_seed_builder
            .compute_next_protocol_parameters()
            .await
            .unwrap();

        assert_eq!(next_protocol_parameters, expected_next_protocol_parameters);
    }

    #[tokio::test]
    async fn test_compute_current_epoch_protocol_message_value() {
        let epoch = Epoch(5);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let signable_seed_builder = build_signable_builder_service(epoch, &fixture, &next_fixture);
        let expected_current_epoch = epoch.to_string();

        let current_epoch = signable_seed_builder.compute_current_epoch().await.unwrap();

        assert_eq!(current_epoch, expected_current_epoch);
    }
}
