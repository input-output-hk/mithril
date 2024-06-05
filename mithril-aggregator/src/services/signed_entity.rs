//! ## SignedEntityService
//!
//! This service is responsible for dealing with [SignedEntity] type.
//! It creates [Artifact] that can be accessed by clients.
use anyhow::Context;
use async_trait::async_trait;
use chrono::Utc;
use slog_scope::info;
use std::sync::Arc;

use mithril_common::{
    entities::{
        BlockNumber, CardanoDbBeacon, CardanoTransactionsSnapshot, Certificate, Epoch,
        MithrilStakeDistribution, SignedEntity, SignedEntityType, SignedEntityTypeDiscriminants,
        Snapshot,
    },
    signable_builder::Artifact,
    StdResult,
};

use crate::{
    artifact_builder::ArtifactBuilder,
    database::{record::SignedEntityRecord, repository::SignedEntityStorer},
};

#[cfg(test)]
use mockall::automock;

/// ArtifactBuilder Service trait
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignedEntityService: Send + Sync {
    /// Create artifact for a signed entity type and a certificate
    async fn create_artifact(
        &self,
        signed_entity_type: SignedEntityType,
        certificate: &Certificate,
    ) -> StdResult<()>;

    /// Return a list of signed snapshots order by creation date descending.
    async fn get_last_signed_snapshots(
        &self,
        total: usize,
    ) -> StdResult<Vec<SignedEntity<Snapshot>>>;

    /// Return a list of signed Mithril stake distribution order by creation
    /// date descending.
    async fn get_last_signed_mithril_stake_distributions(
        &self,
        total: usize,
    ) -> StdResult<Vec<SignedEntity<MithrilStakeDistribution>>>;

    /// Return the last signed Cardano Transaction Snapshot.
    async fn get_last_cardano_transaction_snapshot(
        &self,
    ) -> StdResult<Option<SignedEntity<CardanoTransactionsSnapshot>>>;

    /// Return a signed snapshot
    async fn get_signed_snapshot_by_id(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntity<Snapshot>>>;

    /// Return a signed Mithril stake distribution
    async fn get_signed_mithril_stake_distribution_by_id(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntity<MithrilStakeDistribution>>>;
}

/// Mithril ArtifactBuilder Service
pub struct MithrilSignedEntityService {
    signed_entity_storer: Arc<dyn SignedEntityStorer>,
    mithril_stake_distribution_artifact_builder:
        Arc<dyn ArtifactBuilder<Epoch, MithrilStakeDistribution>>,
    cardano_immutable_files_full_artifact_builder:
        Arc<dyn ArtifactBuilder<CardanoDbBeacon, Snapshot>>,
    cardano_transactions_artifact_builder:
        Arc<dyn ArtifactBuilder<BlockNumber, CardanoTransactionsSnapshot>>,
}

impl MithrilSignedEntityService {
    /// MithrilSignedEntityService factory
    pub fn new(
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
        mithril_stake_distribution_artifact_builder: Arc<
            dyn ArtifactBuilder<Epoch, MithrilStakeDistribution>,
        >,
        cardano_immutable_files_full_artifact_builder: Arc<
            dyn ArtifactBuilder<CardanoDbBeacon, Snapshot>,
        >,
        cardano_transactions_artifact_builder: Arc<
            dyn ArtifactBuilder<BlockNumber, CardanoTransactionsSnapshot>,
        >,
    ) -> Self {
        Self {
            signed_entity_storer,
            mithril_stake_distribution_artifact_builder,
            cardano_immutable_files_full_artifact_builder,
            cardano_transactions_artifact_builder,
        }
    }

    /// Compute artifact from signed entity type
    async fn compute_artifact(
        &self,
        signed_entity_type: SignedEntityType,
        certificate: &Certificate,
    ) -> StdResult<Arc<dyn Artifact>> {
        match signed_entity_type.clone() {
            SignedEntityType::MithrilStakeDistribution(epoch) => Ok(Arc::new(
                self.mithril_stake_distribution_artifact_builder
                    .compute_artifact(epoch, certificate)
                    .await
                    .with_context(|| {
                        format!(
                            "Signed Entity Service can not compute artifact for entity type: '{signed_entity_type}'"
                        )
                    })?,
            )),
            SignedEntityType::CardanoImmutableFilesFull(beacon) => Ok(Arc::new(
                self.cardano_immutable_files_full_artifact_builder
                    .compute_artifact(beacon.clone(), certificate)
                    .await
                    .with_context(|| {
                        format!(
                            "Signed Entity Service can not compute artifact for entity type: '{signed_entity_type}'"
                        )
                    })?,
            )),
            SignedEntityType::CardanoStakeDistribution(_) => todo!(),
            SignedEntityType::CardanoTransactions(_epoch, block_number) => Ok(Arc::new(
                self.cardano_transactions_artifact_builder
                    .compute_artifact(block_number, certificate)
                    .await
                    .with_context(|| {
                        format!(
                            "Signed Entity Service can not compute artifact for entity type: '{signed_entity_type}'"
                        )
                    })?,
            )),
        }
    }

    async fn get_last_signed_entities(
        &self,
        total: usize,
        discriminants: &SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        self.signed_entity_storer
            .get_last_signed_entities_by_type(discriminants, total)
            .await
            .with_context(|| {
                format!(
                    "Signed Entity Service can not get last signed entities with type: '{:?}'",
                    discriminants
                )
            })
    }
}

#[async_trait]
impl SignedEntityService for MithrilSignedEntityService {
    async fn create_artifact(
        &self,
        signed_entity_type: SignedEntityType,
        certificate: &Certificate,
    ) -> StdResult<()> {
        info!(
            "MithrilSignedEntityService::create_artifact";
            "signed_entity_type" => ?signed_entity_type,
            "certificate_hash" => &certificate.hash
        );

        let mut remaining_retries = 2;
        let artifact = loop {
            remaining_retries -= 1;

            match self
                .compute_artifact(signed_entity_type.clone(), certificate)
                .await
            {
                Err(error) if remaining_retries == 0 => break Err(error),
                Err(_error) => (),
                Ok(artifact) => break Ok(artifact),
            };
        }?;

        let signed_entity = SignedEntityRecord {
            signed_entity_id: artifact.get_id(),
            signed_entity_type: signed_entity_type.clone(),
            certificate_id: certificate.hash.clone(),
            artifact: serde_json::to_string(&artifact)?,
            created_at: Utc::now(),
        };

        self.signed_entity_storer
            .store_signed_entity(&signed_entity)
            .await
            .with_context(|| {
                format!(
                    "Signed Entity Service can not store signed entity with type: '{signed_entity_type}'"
                )
            })?;
        Ok(())
    }

    async fn get_last_signed_snapshots(
        &self,
        total: usize,
    ) -> StdResult<Vec<SignedEntity<Snapshot>>> {
        let signed_entities_records = self
            .get_last_signed_entities(
                total,
                &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            )
            .await?;
        let mut signed_entities: Vec<SignedEntity<Snapshot>> = Vec::new();

        for record in signed_entities_records {
            signed_entities.push(record.try_into()?);
        }

        Ok(signed_entities)
    }

    async fn get_last_signed_mithril_stake_distributions(
        &self,
        total: usize,
    ) -> StdResult<Vec<SignedEntity<MithrilStakeDistribution>>> {
        let signed_entities_records = self
            .get_last_signed_entities(
                total,
                &SignedEntityTypeDiscriminants::MithrilStakeDistribution,
            )
            .await?;
        let mut signed_entities: Vec<SignedEntity<MithrilStakeDistribution>> = Vec::new();

        for record in signed_entities_records {
            signed_entities.push(record.try_into()?);
        }

        Ok(signed_entities)
    }

    async fn get_last_cardano_transaction_snapshot(
        &self,
    ) -> StdResult<Option<SignedEntity<CardanoTransactionsSnapshot>>> {
        let mut signed_entities_records = self
            .get_last_signed_entities(1, &SignedEntityTypeDiscriminants::CardanoTransactions)
            .await?;

        match signed_entities_records.pop() {
            Some(record) => Ok(Some(record.try_into()?)),
            None => Ok(None),
        }
    }

    async fn get_signed_snapshot_by_id(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntity<Snapshot>>> {
        let entity: Option<SignedEntity<Snapshot>> = match self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await
            .with_context(|| {
                format!(
                    "Signed Entity Service can not get signed entity with id: '{signed_entity_id}'"
                )
            })? {
            Some(entity) => Some(entity.try_into()?),
            None => None,
        };

        Ok(entity)
    }

    async fn get_signed_mithril_stake_distribution_by_id(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntity<MithrilStakeDistribution>>> {
        let entity: Option<SignedEntity<MithrilStakeDistribution>> = match self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await
            .with_context(|| {
                format!(
                    "Signed Entity Service can not get signed entity with id: '{signed_entity_id}'"
                )
            })? {
            Some(entity) => Some(entity.try_into()?),
            None => None,
        };

        Ok(entity)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        entities::{CardanoTransactionsSnapshot, Epoch},
        signable_builder,
        test_utils::fake_data,
    };
    use serde::{de::DeserializeOwned, Serialize};

    use crate::artifact_builder::MockArtifactBuilder;
    use crate::database::repository::MockSignedEntityStorer;

    use super::*;

    fn create_stake_distribution(epoch: Epoch, signers: usize) -> MithrilStakeDistribution {
        MithrilStakeDistribution::new(
            epoch,
            fake_data::signers_with_stakes(signers),
            &fake_data::protocol_parameters(),
        )
    }

    fn assert_expected<T>(expected: &T, artifact: &Arc<dyn Artifact>)
    where
        T: Serialize + DeserializeOwned,
    {
        let current: T = serde_json::from_str(&serde_json::to_string(&artifact).unwrap()).unwrap();
        assert_eq!(
            serde_json::to_string(&expected).unwrap(),
            serde_json::to_string(&current).unwrap()
        );
    }

    /// Struct that create mocks needed in tests and build objects injecting them.
    struct MockDependencyInjector {
        mock_signed_entity_storer: MockSignedEntityStorer,
        mock_mithril_stake_distribution_artifact_builder:
            MockArtifactBuilder<Epoch, MithrilStakeDistribution>,
        mock_cardano_immutable_files_full_artifact_builder:
            MockArtifactBuilder<CardanoDbBeacon, Snapshot>,
        mock_cardano_transactions_artifact_builder:
            MockArtifactBuilder<BlockNumber, CardanoTransactionsSnapshot>,
    }

    impl MockDependencyInjector {
        fn new() -> MockDependencyInjector {
            MockDependencyInjector {
                mock_signed_entity_storer: MockSignedEntityStorer::new(),
                mock_mithril_stake_distribution_artifact_builder: MockArtifactBuilder::<
                    Epoch,
                    MithrilStakeDistribution,
                >::new(),
                mock_cardano_immutable_files_full_artifact_builder: MockArtifactBuilder::<
                    CardanoDbBeacon,
                    Snapshot,
                >::new(),
                mock_cardano_transactions_artifact_builder: MockArtifactBuilder::<
                    BlockNumber,
                    CardanoTransactionsSnapshot,
                >::new(),
            }
        }

        fn build_artifact_builder_service(self) -> MithrilSignedEntityService {
            MithrilSignedEntityService::new(
                Arc::new(self.mock_signed_entity_storer),
                Arc::new(self.mock_mithril_stake_distribution_artifact_builder),
                Arc::new(self.mock_cardano_immutable_files_full_artifact_builder),
                Arc::new(self.mock_cardano_transactions_artifact_builder),
            )
        }
    }

    #[tokio::test]
    async fn build_mithril_stake_distribution_artifact_when_given_mithril_stake_distribution_entity_type(
    ) {
        let mut mock_container = MockDependencyInjector::new();

        let mithril_stake_distribution_expected = create_stake_distribution(Epoch(1), 5);

        mock_container
            .mock_mithril_stake_distribution_artifact_builder
            .expect_compute_artifact()
            .times(1)
            .returning(|_, _| Ok(create_stake_distribution(Epoch(1), 5)));

        let artifact_builder_service = mock_container.build_artifact_builder_service();

        let certificate = fake_data::certificate("hash".to_string());
        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(1));
        let artifact = artifact_builder_service
            .compute_artifact(signed_entity_type.clone(), &certificate)
            .await
            .unwrap();

        assert_expected(&mithril_stake_distribution_expected, &artifact);
    }

    #[tokio::test]
    async fn should_store_the_artifact_when_creating_artifact_for_a_mithril_stake_distribution() {
        generic_test_that_the_artifact_is_stored(
            SignedEntityType::MithrilStakeDistribution(Epoch(1)),
            create_stake_distribution(Epoch(1), 5),
            &|mock_injector| &mut mock_injector.mock_mithril_stake_distribution_artifact_builder,
        )
        .await;
    }

    #[tokio::test]
    async fn build_snapshot_artifact_when_given_cardano_immutable_files_full_entity_type() {
        let mut mock_container = MockDependencyInjector::new();

        let snapshot_expected = fake_data::snapshots(1).first().unwrap().to_owned();

        mock_container
            .mock_cardano_immutable_files_full_artifact_builder
            .expect_compute_artifact()
            .times(1)
            .returning(|_, _| Ok(fake_data::snapshots(1).first().unwrap().to_owned()));

        let artifact_builder_service = mock_container.build_artifact_builder_service();

        let certificate = fake_data::certificate("hash".to_string());
        let signed_entity_type =
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default());
        let artifact = artifact_builder_service
            .compute_artifact(signed_entity_type.clone(), &certificate)
            .await
            .unwrap();

        assert_expected(&snapshot_expected, &artifact);
    }

    #[tokio::test]
    async fn should_store_the_artifact_when_creating_artifact_for_a_cardano_immutable_files() {
        generic_test_that_the_artifact_is_stored(
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default()),
            fake_data::snapshots(1).first().unwrap().to_owned(),
            &|mock_injector| &mut mock_injector.mock_cardano_immutable_files_full_artifact_builder,
        )
        .await;
    }

    #[tokio::test]
    async fn build_cardano_transactions_snapshot_artifact_when_given_cardano_transactions_type() {
        let mut mock_container = MockDependencyInjector::new();

        let block_number = 151;
        let expected = CardanoTransactionsSnapshot::new("merkle_root".to_string(), block_number);

        mock_container
            .mock_cardano_transactions_artifact_builder
            .expect_compute_artifact()
            .times(1)
            .returning(move |_, _| {
                Ok(CardanoTransactionsSnapshot::new(
                    "merkle_root".to_string(),
                    block_number,
                ))
            });

        let artifact_builder_service = mock_container.build_artifact_builder_service();

        let certificate = fake_data::certificate("hash".to_string());
        let signed_entity_type = SignedEntityType::CardanoTransactions(Epoch(1), block_number);
        let artifact = artifact_builder_service
            .compute_artifact(signed_entity_type.clone(), &certificate)
            .await
            .unwrap();

        assert_expected(&expected, &artifact);
    }

    #[tokio::test]
    async fn should_store_the_artifact_when_creating_artifact_for_cardano_transactions() {
        let block_number = 149;
        generic_test_that_the_artifact_is_stored(
            SignedEntityType::CardanoTransactions(Epoch(1), block_number),
            CardanoTransactionsSnapshot::new("merkle_root".to_string(), block_number),
            &|mock_injector| &mut mock_injector.mock_cardano_transactions_artifact_builder,
        )
        .await;
    }

    async fn generic_test_that_the_artifact_is_stored<
        T: Artifact + Clone + Serialize + 'static,
        U: signable_builder::Beacon,
    >(
        signed_entity_type: SignedEntityType,
        artifact: T,
        mock_that_provide_artifact: &dyn Fn(
            &mut MockDependencyInjector,
        ) -> &mut MockArtifactBuilder<U, T>,
    ) {
        let mut mock_container = MockDependencyInjector::new();
        {
            let artifact_clone: Arc<dyn Artifact> = Arc::new(artifact.clone());
            let signed_entity_artifact = serde_json::to_string(&artifact_clone).unwrap();
            mock_container
                .mock_signed_entity_storer
                .expect_store_signed_entity()
                .withf(move |signed_entity| signed_entity.artifact == signed_entity_artifact)
                .return_once(|_| Ok(()));
        }
        {
            let artifact_cloned = artifact.clone();
            mock_that_provide_artifact(&mut mock_container)
                .expect_compute_artifact()
                .times(1)
                .return_once(|_, _| Ok(artifact_cloned));
        }
        let artifact_builder_service = mock_container.build_artifact_builder_service();

        let certificate = fake_data::certificate("hash".to_string());
        let error_message = format!(
            "Create artifact should not fail for {} signed entity",
            std::any::type_name::<T>()
        );
        let error_message_str = error_message.as_str();

        artifact_builder_service
            .create_artifact(signed_entity_type, &certificate)
            .await
            .expect(error_message_str);
    }
}
