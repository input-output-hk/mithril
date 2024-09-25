use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;

use crate::{
    entities::{
        BlockNumber, CardanoDbBeacon, Epoch, ProtocolMessage, ProtocolMessagePartKey,
        SignedEntityType,
    },
    era::{EraChecker, SupportedEra},
    signable_builder::{SignableBuilder, SignableSeedBuilder},
    StdResult,
};

/// ArtifactBuilder Service trait
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignableBuilderService: Send + Sync {
    /// Compute signable from signed entity type
    async fn compute_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage>;
}

/// Mithril Signable Builder Service
pub struct MithrilSignableBuilderService {
    era_checker: Arc<EraChecker>,
    seed_signable_builder: Arc<dyn SignableSeedBuilder>,
    mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
    cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
}

impl MithrilSignableBuilderService {
    /// MithrilSignableBuilderService factory
    pub fn new(
        era_checker: Arc<EraChecker>,
        seed_signable_builder: Arc<dyn SignableSeedBuilder>,
        mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
        immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
        cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
        cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    ) -> Self {
        Self {
            era_checker,
            seed_signable_builder,
            mithril_stake_distribution_builder,
            immutable_signable_builder,
            cardano_transactions_signable_builder,
            cardano_stake_distribution_builder,
        }
    }

    async fn compute_signed_entity_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        let protocol_message = match signed_entity_type {
            SignedEntityType::MithrilStakeDistribution(e) => self
                .mithril_stake_distribution_builder
                .compute_protocol_message(e)
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with epoch: '{e}'"
                ))?,
            SignedEntityType::CardanoImmutableFilesFull(beacon) => self
                .immutable_signable_builder
                .compute_protocol_message(beacon.clone())
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with beacon: '{beacon}'"
                ))?,
            SignedEntityType::CardanoStakeDistribution(e) => self
                .cardano_stake_distribution_builder
                .compute_protocol_message(e)
                .await
                .with_context(|| "Signable builder service can not compute protocol message for Cardano stake distribution with epoch: '{e}")?,
            SignedEntityType::CardanoTransactions(_, block_number) => self
                .cardano_transactions_signable_builder
                .compute_protocol_message(block_number)
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with block_number: '{block_number}'"
                ))?,
        };

        Ok(protocol_message)
    }

    async fn compute_seeded_protocol_message(
        &self,
        protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage> {
        let mut protocol_message = protocol_message;
        let next_aggregate_verification_key = self
            .seed_signable_builder
            .compute_next_aggregate_verification_key_protocol_message_part_value()
            .await?;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            next_aggregate_verification_key,
        );

        if matches!(self.era_checker.current_era(), SupportedEra::Pythagoras) {
            let next_protocol_parameters = self
                .seed_signable_builder
                .compute_next_protocol_parameters_protocol_message_part_value()
                .await?;
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextProtocolParameters,
                next_protocol_parameters,
            );
            let current_epoch = self
                .seed_signable_builder
                .compute_current_epoch_protocol_message_part_value()
                .await?;
            protocol_message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, current_epoch);
        }

        Ok(protocol_message)
    }
}

#[async_trait]
impl SignableBuilderService for MithrilSignableBuilderService {
    async fn compute_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        let protocol_message = self
            .compute_signed_entity_protocol_message(signed_entity_type)
            .await?;
        let protocol_message = self
            .compute_seeded_protocol_message(protocol_message)
            .await?;

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        entities::{BlockNumber, Epoch, ProtocolMessage},
        era::SupportedEra,
        signable_builder::{Beacon as Beaconnable, MockSignableSeedBuilder, SignableBuilder},
        StdResult,
    };

    use async_trait::async_trait;
    use mockall::mock;

    mock! {
        SignableBuilderImpl<U> { }

        #[async_trait]
        impl<U> SignableBuilder<U> for SignableBuilderImpl<U> where U: Beaconnable,
        {
            async fn compute_protocol_message(&self, beacon: U) -> StdResult<ProtocolMessage>;
        }
    }

    struct MockDependencyInjector {
        era_checker: EraChecker,
        mock_signable_seed_builder: MockSignableSeedBuilder,
        mock_mithril_stake_distribution_signable_builder: MockSignableBuilderImpl<Epoch>,
        mock_cardano_immutable_files_full_signable_builder:
            MockSignableBuilderImpl<CardanoDbBeacon>,
        mock_cardano_transactions_signable_builder: MockSignableBuilderImpl<BlockNumber>,
        mock_cardano_stake_distribution_signable_builder: MockSignableBuilderImpl<Epoch>,
    }

    impl MockDependencyInjector {
        fn new(current_era: SupportedEra) -> MockDependencyInjector {
            MockDependencyInjector {
                era_checker: EraChecker::new(current_era, Epoch(1)),
                mock_signable_seed_builder: MockSignableSeedBuilder::new(),
                mock_mithril_stake_distribution_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_immutable_files_full_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_stake_distribution_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_transactions_signable_builder: MockSignableBuilderImpl::new(),
            }
        }

        fn build_signable_builder_service(self) -> MithrilSignableBuilderService {
            MithrilSignableBuilderService::new(
                Arc::new(self.era_checker),
                Arc::new(self.mock_signable_seed_builder),
                Arc::new(self.mock_mithril_stake_distribution_signable_builder),
                Arc::new(self.mock_cardano_immutable_files_full_signable_builder),
                Arc::new(self.mock_cardano_transactions_signable_builder),
                Arc::new(self.mock_cardano_stake_distribution_signable_builder),
            )
        }
    }

    mod pythagoras_era {
        use super::*;

        #[tokio::test]
        async fn build_mithril_stake_distribution_signable_when_given_mithril_stake_distribution_entity_type(
        ) {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Pythagoras;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_protocol_parameters_protocol_message_part_value()
                .once()
                .return_once(move || Ok("protocol-params-hash-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_current_epoch_protocol_message_part_value()
                .once()
                .return_once(move || Ok("epoch-123".to_string()));
            mock_container
                .mock_mithril_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(1));

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn build_snapshot_signable_when_given_cardano_immutable_files_full_entity_type() {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Pythagoras;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_protocol_parameters_protocol_message_part_value()
                .once()
                .return_once(move || Ok("protocol-params-hash-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_current_epoch_protocol_message_part_value()
                .once()
                .return_once(move || Ok("epoch-123".to_string()));
            mock_container
                .mock_cardano_immutable_files_full_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type =
                SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default());

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn build_transactions_signable_when_given_cardano_transactions_entity_type() {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Pythagoras;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_protocol_parameters_protocol_message_part_value()
                .once()
                .return_once(move || Ok("protocol-params-hash-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_current_epoch_protocol_message_part_value()
                .once()
                .return_once(move || Ok("epoch-123".to_string()));
            mock_container
                .mock_cardano_transactions_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type =
                SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(1000));

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn build_cardano_stake_distribution_signable_when_given_cardano_stake_distribution_entity_type(
        ) {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Pythagoras;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_protocol_parameters_protocol_message_part_value()
                .once()
                .return_once(move || Ok("protocol-params-hash-123".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_current_epoch_protocol_message_part_value()
                .once()
                .return_once(move || Ok("epoch-123".to_string()));
            mock_container
                .mock_cardano_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type = SignedEntityType::CardanoStakeDistribution(Epoch(5));

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }
    }

    mod thales_era {
        use super::*;

        #[tokio::test]
        async fn build_mithril_stake_distribution_signable_when_given_mithril_stake_distribution_entity_type(
        ) {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Thales;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_mithril_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(1));

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn build_snapshot_signable_when_given_cardano_immutable_files_full_entity_type() {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Thales;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_cardano_immutable_files_full_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type =
                SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default());

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn build_transactions_signable_when_given_cardano_transactions_entity_type() {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Thales;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_cardano_transactions_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type =
                SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(1000));

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn build_cardano_stake_distribution_signable_when_given_cardano_stake_distribution_entity_type(
        ) {
            let protocol_message = ProtocolMessage::new();
            let protocol_message_clone = protocol_message.clone();
            let current_era = SupportedEra::Thales;
            let mut mock_container = MockDependencyInjector::new(current_era);
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_protocol_message_part_value()
                .once()
                .return_once(move || Ok("next-avk-123".to_string()));
            mock_container
                .mock_cardano_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(move |_| Ok(protocol_message_clone));
            let signable_builder_service = mock_container.build_signable_builder_service();
            let signed_entity_type = SignedEntityType::CardanoStakeDistribution(Epoch(5));

            signable_builder_service
                .compute_protocol_message(signed_entity_type)
                .await
                .unwrap();
        }
    }
}
