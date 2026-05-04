use anyhow::Context;
use async_trait::async_trait;
use slog::{Logger, debug};
use std::sync::Arc;

use crate::{
    StdResult,
    entities::{
        BlockNumber, BlockNumberOffset, CardanoDbBeacon, Epoch, ProtocolMessage,
        ProtocolMessagePartKey, SignedEntityType,
    },
    logging::LoggerExtensions,
    signable_builder::{SignableBuilder, SignableSeedBuilder},
};

#[cfg(feature = "future_snark")]
use crate::entities::SupportedEra;

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
    seed_signable_builder: Arc<dyn SignableSeedBuilder>,
    mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
    cardano_blocks_transactions_signable_builder:
        Arc<dyn SignableBuilder<(BlockNumber, BlockNumberOffset)>>,
    cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    cardano_database_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    logger: Logger,
}

/// SignableBuilders dependencies required by the [MithrilSignableBuilderService].
pub struct SignableBuilderServiceDependencies {
    mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
    cardano_blocks_transactions_signable_builder:
        Arc<dyn SignableBuilder<(BlockNumber, BlockNumberOffset)>>,
    cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    cardano_database_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
}

impl SignableBuilderServiceDependencies {
    /// Create a new instance of [SignableBuilderServiceDependencies].
    pub fn new(
        mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
        immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
        cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
        cardano_blocks_transactions_signable_builder: Arc<
            dyn SignableBuilder<(BlockNumber, BlockNumberOffset)>,
        >,
        cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
        cardano_database_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    ) -> Self {
        Self {
            mithril_stake_distribution_builder,
            immutable_signable_builder,
            cardano_transactions_signable_builder,
            cardano_blocks_transactions_signable_builder,
            cardano_stake_distribution_builder,
            cardano_database_signable_builder,
        }
    }
}

impl MithrilSignableBuilderService {
    /// MithrilSignableBuilderService factory
    pub fn new(
        seed_signable_builder: Arc<dyn SignableSeedBuilder>,
        dependencies: SignableBuilderServiceDependencies,
        logger: Logger,
    ) -> Self {
        Self {
            seed_signable_builder,
            mithril_stake_distribution_builder: dependencies.mithril_stake_distribution_builder,
            immutable_signable_builder: dependencies.immutable_signable_builder,
            cardano_transactions_signable_builder: dependencies
                .cardano_transactions_signable_builder,
            cardano_blocks_transactions_signable_builder: dependencies
                .cardano_blocks_transactions_signable_builder,
            cardano_stake_distribution_builder: dependencies.cardano_stake_distribution_builder,
            cardano_database_signable_builder: dependencies.cardano_database_signable_builder,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn compute_signed_entity_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        debug!(
            self.logger,
            "Compute protocol message for signed entity type: '{signed_entity_type:?}'"
        );

        let protocol_message = match &signed_entity_type {
            SignedEntityType::MithrilStakeDistribution(e) => {
                self.mithril_stake_distribution_builder
                    .compute_protocol_message(*e)
                    .await
            }
            SignedEntityType::CardanoImmutableFilesFull(beacon) => {
                self.immutable_signable_builder
                    .compute_protocol_message(beacon.clone())
                    .await
            }
            SignedEntityType::CardanoStakeDistribution(e) => {
                self.cardano_stake_distribution_builder
                    .compute_protocol_message(*e)
                    .await
            }
            SignedEntityType::CardanoTransactions(_, block_number) => {
                self.cardano_transactions_signable_builder
                    .compute_protocol_message(*block_number)
                    .await
            }
            SignedEntityType::CardanoBlocksTransactions(_, block_number, block_number_offset) => {
                self.cardano_blocks_transactions_signable_builder
                    .compute_protocol_message((*block_number, *block_number_offset))
                    .await
            }
            SignedEntityType::CardanoDatabase(beacon) => {
                self.cardano_database_signable_builder
                    .compute_protocol_message(beacon.clone())
                    .await
            }
        }
        .with_context(|| {
            format!("Signable builder service can not compute protocol message for signed entity type: '{signed_entity_type:?}'")
        })?;

        Ok(protocol_message)
    }

    async fn compute_seeded_protocol_message(
        &self,
        protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage> {
        let mut protocol_message = protocol_message;
        let next_aggregate_verification_key = self
            .seed_signable_builder
            .compute_next_aggregate_verification_key_for_concatenation()
            .await?;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            next_aggregate_verification_key,
        );

        #[cfg(feature = "future_snark")]
        let next_snark_aggregate_verification_key = self
            .seed_signable_builder
            .compute_next_aggregate_verification_key_for_snark()
            .await?;
        #[cfg(feature = "future_snark")]
        if let Some(snark_avk) = next_snark_aggregate_verification_key.clone() {
            protocol_message.set_message_part(
                ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
                snark_avk,
            );
        }

        let next_protocol_parameters =
            self.seed_signable_builder.compute_next_protocol_parameters().await?;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextProtocolParameters,
            next_protocol_parameters,
        );
        let current_epoch = self.seed_signable_builder.compute_current_epoch().await?;
        protocol_message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, current_epoch);

        #[cfg(feature = "future_snark")]
        {
            let era = self.seed_signable_builder.compute_current_era().await?;
            match era {
                SupportedEra::Lagrange => {
                    if next_snark_aggregate_verification_key.is_none() {
                        anyhow::bail!(
                            "Signable builder service: Lagrange era requires a SNARK aggregate verification key but none was computed"
                        );
                    }
                    protocol_message.hash_scheme =
                        crate::entities::ProtocolMessageHashScheme::Rigid;
                }
                SupportedEra::Pythagoras => {}
            }
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
        let protocol_message = self.compute_seeded_protocol_message(protocol_message).await?;
        #[cfg(feature = "future_snark")]
        protocol_message.check_rigid_integrity().with_context(
            || "Signable builder service produced a protocol message that violates the rigid layout",
        )?;

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        StdResult,
        entities::{BlockNumber, BlockNumberOffset, Epoch, ProtocolMessage},
        signable_builder::{Beacon as Beaconnable, MockSignableSeedBuilder, SignableBuilder},
        test::TestLogger,
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
        mock_signable_seed_builder: MockSignableSeedBuilder,
        mock_mithril_stake_distribution_signable_builder: MockSignableBuilderImpl<Epoch>,
        mock_cardano_immutable_files_full_signable_builder:
            MockSignableBuilderImpl<CardanoDbBeacon>,
        mock_cardano_transactions_signable_builder: MockSignableBuilderImpl<BlockNumber>,
        mock_cardano_blocks_transactions_signable_builder:
            MockSignableBuilderImpl<(BlockNumber, BlockNumberOffset)>,
        mock_cardano_stake_distribution_signable_builder: MockSignableBuilderImpl<Epoch>,
        mock_cardano_database_signable_builder: MockSignableBuilderImpl<CardanoDbBeacon>,
    }

    impl MockDependencyInjector {
        fn new() -> MockDependencyInjector {
            MockDependencyInjector {
                mock_signable_seed_builder: MockSignableSeedBuilder::new(),
                mock_mithril_stake_distribution_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_immutable_files_full_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_transactions_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_blocks_transactions_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_stake_distribution_signable_builder: MockSignableBuilderImpl::new(),
                mock_cardano_database_signable_builder: MockSignableBuilderImpl::new(),
            }
        }

        fn build_signable_builder_service(self) -> MithrilSignableBuilderService {
            let dependencies = SignableBuilderServiceDependencies::new(
                Arc::new(self.mock_mithril_stake_distribution_signable_builder),
                Arc::new(self.mock_cardano_immutable_files_full_signable_builder),
                Arc::new(self.mock_cardano_transactions_signable_builder),
                Arc::new(self.mock_cardano_blocks_transactions_signable_builder),
                Arc::new(self.mock_cardano_stake_distribution_signable_builder),
                Arc::new(self.mock_cardano_database_signable_builder),
            );

            MithrilSignableBuilderService::new(
                Arc::new(self.mock_signable_seed_builder),
                dependencies,
                TestLogger::stdout(),
            )
        }
    }

    fn build_mock_container() -> MockDependencyInjector {
        let mut mock_container = MockDependencyInjector::new();
        mock_container
            .mock_signable_seed_builder
            .expect_compute_next_aggregate_verification_key_for_concatenation()
            .once()
            .return_once(move || Ok("next-avk-123".to_string()));
        #[cfg(feature = "future_snark")]
        mock_container
            .mock_signable_seed_builder
            .expect_compute_next_aggregate_verification_key_for_snark()
            .once()
            .return_once(move || Ok(Some("next-snark-avk-123".to_string())));
        mock_container
            .mock_signable_seed_builder
            .expect_compute_next_protocol_parameters()
            .once()
            .return_once(move || Ok("protocol-params-hash-123".to_string()));
        mock_container
            .mock_signable_seed_builder
            .expect_compute_current_epoch()
            .once()
            .return_once(move || Ok("epoch-123".to_string()));
        #[cfg(feature = "future_snark")]
        mock_container
            .mock_signable_seed_builder
            .expect_compute_current_era()
            .once()
            .return_once(move || Ok(crate::entities::SupportedEra::Pythagoras));

        mock_container
    }

    #[tokio::test]
    async fn build_mithril_stake_distribution_signable_when_given_mithril_stake_distribution_entity_type()
     {
        let mut mock_container = build_mock_container();
        mock_container
            .mock_mithril_stake_distribution_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(|_| Ok(ProtocolMessage::new()));
        let signable_builder_service = mock_container.build_signable_builder_service();
        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(1));

        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_snapshot_signable_when_given_cardano_immutable_files_full_entity_type() {
        let mut mock_container = build_mock_container();
        mock_container
            .mock_cardano_immutable_files_full_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(|_| Ok(ProtocolMessage::new()));
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
        let mut mock_container = build_mock_container();
        mock_container
            .mock_cardano_transactions_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(|_| Ok(ProtocolMessage::new()));
        let signable_builder_service = mock_container.build_signable_builder_service();
        let signed_entity_type = SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(1000));

        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_blocks_transactions_signable_when_given_cardano_blocks_transactions_entity_type()
    {
        let mut mock_container = build_mock_container();
        mock_container
            .mock_cardano_blocks_transactions_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(|_| Ok(ProtocolMessage::new()));
        let signable_builder_service = mock_container.build_signable_builder_service();
        let signed_entity_type = SignedEntityType::CardanoBlocksTransactions(
            Epoch(6),
            BlockNumber(1010),
            BlockNumberOffset(15),
        );

        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_cardano_stake_distribution_signable_when_given_cardano_stake_distribution_entity_type()
     {
        let mut mock_container = build_mock_container();
        mock_container
            .mock_cardano_stake_distribution_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(|_| Ok(ProtocolMessage::new()));
        let signable_builder_service = mock_container.build_signable_builder_service();
        let signed_entity_type = SignedEntityType::CardanoStakeDistribution(Epoch(5));

        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_cardano_database_signable_when_given_cardano_database_entity_type() {
        let mut mock_container = build_mock_container();
        mock_container
            .mock_cardano_database_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(|_| Ok(ProtocolMessage::new()));
        let signable_builder_service = mock_container.build_signable_builder_service();
        let signed_entity_type = SignedEntityType::CardanoDatabase(CardanoDbBeacon::default());

        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[cfg(feature = "future_snark")]
    mod era_dispatch {
        use super::*;

        use crate::entities::{ProtocolMessagePartKey, SupportedEra};

        fn valid_snark_aggregate_verification_key_wire_value() -> String {
            let mut bytes = Vec::with_capacity(40);
            bytes.extend_from_slice(&[0xCDu8; 32]);
            bytes.extend_from_slice(&0u64.to_be_bytes());
            hex::encode(bytes)
        }

        fn build_mock_container_for_era(
            era: SupportedEra,
            snark_aggregate_verification_key: Option<String>,
        ) -> MockDependencyInjector {
            let mut mock_container = MockDependencyInjector::new();
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_for_concatenation()
                .once()
                .return_once(move || Ok(hex::encode(vec![0xABu8; 44])));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_aggregate_verification_key_for_snark()
                .once()
                .return_once(move || Ok(snark_aggregate_verification_key));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_next_protocol_parameters()
                .once()
                .return_once(move || Ok(hex::encode(vec![0xEFu8; 32])));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_current_epoch()
                .once()
                .return_once(move || Ok("7".to_string()));
            mock_container
                .mock_signable_seed_builder
                .expect_compute_current_era()
                .once()
                .return_once(move || Ok(era));
            mock_container
        }

        #[tokio::test]
        async fn pythagoras_era_keeps_the_legacy_protocol_message_hash_scheme() {
            let mut mock_container = build_mock_container_for_era(
                SupportedEra::Pythagoras,
                Some(valid_snark_aggregate_verification_key_wire_value()),
            );
            mock_container
                .mock_mithril_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(|_| {
                    let mut message = ProtocolMessage::new();
                    message.set_message_part(
                        ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
                        hex::encode(b"merkle-root"),
                    );
                    Ok(message)
                });
            let signable_builder_service = mock_container.build_signable_builder_service();

            let protocol_message = signable_builder_service
                .compute_protocol_message(SignedEntityType::MithrilStakeDistribution(Epoch(7)))
                .await
                .unwrap();

            assert!(
                !protocol_message.is_rigid(),
                "Pythagoras era must keep the legacy protocol message version"
            );
        }

        #[tokio::test]
        async fn lagrange_era_switches_the_protocol_message_to_the_rigid_hash_scheme() {
            let mut mock_container = build_mock_container_for_era(
                SupportedEra::Lagrange,
                Some(valid_snark_aggregate_verification_key_wire_value()),
            );
            mock_container
                .mock_mithril_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(|_| {
                    let mut message = ProtocolMessage::new();
                    message.set_message_part(
                        ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
                        hex::encode(b"merkle-root"),
                    );
                    Ok(message)
                });
            let signable_builder_service = mock_container.build_signable_builder_service();

            let protocol_message = signable_builder_service
                .compute_protocol_message(SignedEntityType::MithrilStakeDistribution(Epoch(7)))
                .await
                .unwrap();

            assert!(
                protocol_message.is_rigid(),
                "Lagrange era must flip the protocol message to the rigid version"
            );
            assert_eq!(
                protocol_message.get_current_epoch(),
                Some(Epoch(7)),
                "the rigid message must still expose the typed current epoch accessor"
            );
            assert!(
                protocol_message.has_next_snark_aggregate_verification_key(),
                "the seed builder must have injected the SNARK AVK into the rigid message"
            );
        }

        #[tokio::test]
        async fn lagrange_era_without_a_snark_aggregate_verification_key_yields_an_error() {
            let mut mock_container = build_mock_container_for_era(SupportedEra::Lagrange, None);
            mock_container
                .mock_mithril_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(|_| Ok(ProtocolMessage::new()));
            let signable_builder_service = mock_container.build_signable_builder_service();

            signable_builder_service
                .compute_protocol_message(SignedEntityType::MithrilStakeDistribution(Epoch(7)))
                .await
                .expect_err(
                    "Lagrange era without a SNARK aggregate verification key must be rejected",
                );
        }

        #[tokio::test]
        async fn lagrange_era_with_an_ill_formed_rigid_field_is_rejected_by_the_integrity_check() {
            let mut mock_container = build_mock_container_for_era(
                SupportedEra::Lagrange,
                Some("not-a-valid-snark-avk-encoding".to_string()),
            );
            mock_container
                .mock_mithril_stake_distribution_signable_builder
                .expect_compute_protocol_message()
                .once()
                .return_once(|_| {
                    let mut message = ProtocolMessage::new();
                    message.set_message_part(
                        ProtocolMessagePartKey::CardanoStakeDistributionMerkleRoot,
                        hex::encode(b"merkle-root"),
                    );
                    Ok(message)
                });
            let signable_builder_service = mock_container.build_signable_builder_service();

            let error = signable_builder_service
                .compute_protocol_message(SignedEntityType::MithrilStakeDistribution(Epoch(7)))
                .await
                .expect_err(
                    "rigid layout violation must be surfaced by the signable builder integrity check",
                );

            let integrity_error = error
                .downcast_ref::<crate::entities::RigidProtocolMessageIntegrityError>()
                .expect("the signable builder must surface a `RigidProtocolMessageIntegrityError`");
            assert!(
                matches!(
                    integrity_error,
                    crate::entities::RigidProtocolMessageIntegrityError::InvalidSnarkAggregateVerificationKey(_)
                ),
                "unexpected error variant: {integrity_error:?}"
            );
        }
    }
}
