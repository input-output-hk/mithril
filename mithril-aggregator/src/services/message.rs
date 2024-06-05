//! This service is responsible for providing HTTP server with messages as fast as possible.

use std::sync::Arc;

use async_trait::async_trait;
use thiserror::Error;

use mithril_common::{
    entities::SignedEntityTypeDiscriminants,
    messages::{
        CardanoTransactionSnapshotListMessage, CardanoTransactionSnapshotMessage,
        CertificateListMessage, CertificateMessage, MithrilStakeDistributionListMessage,
        MithrilStakeDistributionMessage, SnapshotListMessage, SnapshotMessage,
    },
    StdResult,
};

use crate::database::repository::{CertificateRepository, SignedEntityStorer};

#[cfg(test)]
use mockall::automock;

/// Error related to the [MessageService]
#[derive(Debug, Error)]
pub enum MessageServiceError {
    /// There is no current PendingCertificate
    #[error("There is no current pending certificate.")]
    PendingCertificateDoesNotExist,
}
/// HTTP Message service trait.
#[cfg_attr(test, automock)]
#[async_trait]
pub trait MessageService: Sync + Send {
    /// Return the message representation of a certificate if it exists.
    async fn get_certificate_message(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>>;

    /// Return the message representation of the last N certificates
    async fn get_certificate_list_message(&self, limit: usize)
        -> StdResult<CertificateListMessage>;

    /// Return the information regarding the given snapshot
    async fn get_snapshot_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SnapshotMessage>>;

    /// Return the list of the last signed snapshots. The limit of the list is
    /// passed as argument.
    async fn get_snapshot_list_message(&self, limit: usize) -> StdResult<SnapshotListMessage>;

    /// Return the information regarding the MSD for the given identifier.
    async fn get_mithril_stake_distribution_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<MithrilStakeDistributionMessage>>;

    /// Return the list of the last Mithril stake distributions message
    async fn get_mithril_stake_distribution_list_message(
        &self,
        limit: usize,
    ) -> StdResult<MithrilStakeDistributionListMessage>;

    /// Return the information regarding the Cardano transactions set for the given identifier.
    async fn get_cardano_transaction_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoTransactionSnapshotMessage>>;

    /// Return the list of the last Cardano transactions set message
    async fn get_cardano_transaction_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoTransactionSnapshotListMessage>;
}

/// Implementation of the [MessageService]
pub struct MithrilMessageService {
    certificate_repository: Arc<CertificateRepository>,
    signed_entity_storer: Arc<dyn SignedEntityStorer>,
}

impl MithrilMessageService {
    /// Constructor
    pub fn new(
        certificate_repository: Arc<CertificateRepository>,
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
    ) -> Self {
        Self {
            certificate_repository,
            signed_entity_storer,
        }
    }
}

#[async_trait]
impl MessageService for MithrilMessageService {
    async fn get_certificate_message(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>> {
        self.certificate_repository
            .get_certificate(certificate_hash)
            .await
    }

    async fn get_certificate_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CertificateListMessage> {
        self.certificate_repository
            .get_latest_certificates(limit)
            .await
    }

    async fn get_snapshot_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SnapshotMessage>> {
        let signed_entity = self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await?;

        signed_entity.map(|s| s.try_into()).transpose()
    }

    async fn get_snapshot_list_message(&self, limit: usize) -> StdResult<SnapshotListMessage> {
        let signed_entity_type_id = SignedEntityTypeDiscriminants::CardanoImmutableFilesFull;
        let entities = self
            .signed_entity_storer
            .get_last_signed_entities_by_type(&signed_entity_type_id, limit)
            .await?;

        entities.into_iter().map(|i| i.try_into()).collect()
    }

    async fn get_mithril_stake_distribution_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<MithrilStakeDistributionMessage>> {
        let signed_entity = self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await?;

        signed_entity.map(|v| v.try_into()).transpose()
    }

    async fn get_mithril_stake_distribution_list_message(
        &self,
        limit: usize,
    ) -> StdResult<MithrilStakeDistributionListMessage> {
        let signed_entity_type_id = SignedEntityTypeDiscriminants::MithrilStakeDistribution;
        let entities = self
            .signed_entity_storer
            .get_last_signed_entities_by_type(&signed_entity_type_id, limit)
            .await?;

        entities.into_iter().map(|i| i.try_into()).collect()
    }

    async fn get_cardano_transaction_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoTransactionSnapshotMessage>> {
        let signed_entity = self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await?;

        signed_entity.map(|v| v.try_into()).transpose()
    }

    async fn get_cardano_transaction_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoTransactionSnapshotListMessage> {
        let signed_entity_type_id = SignedEntityTypeDiscriminants::CardanoTransactions;
        let entities = self
            .signed_entity_storer
            .get_last_signed_entities_by_type(&signed_entity_type_id, limit)
            .await?;

        entities.into_iter().map(|i| i.try_into()).collect()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use mithril_common::entities::{
        CardanoTransactionsSnapshot, Certificate, Epoch, MithrilStakeDistribution, SignedEntity,
        SignedEntityType, Snapshot,
    };
    use mithril_common::messages::ToMessageAdapter;
    use mithril_common::test_utils::MithrilFixtureBuilder;

    use crate::database::record::SignedEntityRecord;
    use crate::database::repository::MockSignedEntityStorer;
    use crate::dependency_injection::DependenciesBuilder;
    use crate::message_adapters::{
        ToCardanoTransactionListMessageAdapter, ToCardanoTransactionMessageAdapter,
        ToMithrilStakeDistributionListMessageAdapter, ToMithrilStakeDistributionMessageAdapter,
        ToSnapshotListMessageAdapter, ToSnapshotMessageAdapter,
    };
    use crate::Configuration;

    #[tokio::test]
    async fn get_no_certificate() {
        // setup
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let service = dep_builder.get_message_service().await.unwrap();

        // test
        let certificate_hash = "whatever";
        let certficate_message = service
            .get_certificate_message(certificate_hash)
            .await
            .unwrap();
        assert!(certficate_message.is_none());
    }

    #[tokio::test]
    async fn get_certificate() {
        // setup
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let repository = dep_builder.get_certificate_repository().await.unwrap();
        let service = dep_builder.get_message_service().await.unwrap();
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let genesis_certificate = fixture.create_genesis_certificate("whatever", Epoch(2), 1);
        repository
            .create_certificate(genesis_certificate.clone())
            .await
            .unwrap();

        // test
        let certificate_message = service
            .get_certificate_message(&genesis_certificate.hash)
            .await
            .unwrap()
            .expect("There should be a certificate.");
        assert_eq!(genesis_certificate.hash, certificate_message.hash);
    }

    #[tokio::test]
    async fn get_last_certificates() {
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let repository = dep_builder.get_certificate_repository().await.unwrap();
        let service = dep_builder.get_message_service().await.unwrap();
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();

        let certificates: Vec<Certificate> = [2, 3]
            .into_iter()
            .map(|epoch| fixture.create_genesis_certificate("whatever", Epoch(epoch), 1))
            .collect();
        let last_certificate_hash = certificates[1].hash.clone();
        repository
            .create_many_certificates(certificates.clone())
            .await
            .unwrap();

        // test
        let certificate_messages = service.get_certificate_list_message(5).await.unwrap();

        assert_eq!(2, certificate_messages.len());
        assert_eq!(last_certificate_hash, certificate_messages[0].hash);
    }

    #[tokio::test]
    async fn get_snapshot_not_exist() {
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let service = dep_builder.get_message_service().await.unwrap();
        let snapshot = service.get_snapshot_message("whatever").await.unwrap();

        assert!(snapshot.is_none());
    }

    #[tokio::test]
    async fn get_snapshot() {
        let entity = SignedEntity::<Snapshot>::dummy();
        let record = SignedEntityRecord {
            signed_entity_id: entity.signed_entity_id.clone(),
            signed_entity_type: entity.signed_entity_type.clone(),
            certificate_id: entity.certificate_id.clone(),
            artifact: serde_json::to_string(&entity.artifact).unwrap(),
            created_at: entity.created_at,
        };
        let message = ToSnapshotMessageAdapter::adapt(entity);

        // setup
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(Some(record)))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_snapshot_message("whatever")
            .await
            .unwrap()
            .expect("A SnapshotMessage was expected.");

        assert_eq!(message, response);
    }

    #[tokio::test]
    async fn get_snapshot_list_message() {
        let entity = SignedEntity::<Snapshot>::dummy();
        let records = vec![SignedEntityRecord {
            signed_entity_id: entity.signed_entity_id.clone(),
            signed_entity_type: entity.signed_entity_type.clone(),
            certificate_id: entity.certificate_id.clone(),
            artifact: serde_json::to_string(&entity.artifact).unwrap(),
            created_at: entity.created_at,
        }];
        let entities = vec![entity];
        let message = ToSnapshotListMessageAdapter::adapt(entities);

        // setup
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_last_signed_entities_by_type()
            .return_once(|_, _| Ok(records))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service.get_snapshot_list_message(3).await.unwrap();

        assert_eq!(message, response);
    }

    #[tokio::test]
    async fn get_mithril_stake_distribution() {
        let entity = SignedEntity::<MithrilStakeDistribution>::dummy();
        let record = SignedEntityRecord {
            signed_entity_id: entity.signed_entity_id.clone(),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(entity.artifact.epoch),
            certificate_id: entity.certificate_id.clone(),
            artifact: serde_json::to_string(&entity.artifact).unwrap(),
            created_at: entity.created_at,
        };
        let message = ToMithrilStakeDistributionMessageAdapter::adapt(entity);
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(Some(record)))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_mithril_stake_distribution_message("whatever")
            .await
            .unwrap()
            .expect("A MithrilStakeDistributionMessage was expected.");

        assert_eq!(message, response);
    }

    #[tokio::test]
    async fn get_mithril_stake_distribution_not_exist() {
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(None))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_mithril_stake_distribution_message("whatever")
            .await
            .unwrap();

        assert!(response.is_none());
    }

    #[tokio::test]
    async fn get_mithril_stake_distribution_list_message() {
        let entity = SignedEntity::<MithrilStakeDistribution>::dummy();
        let records = vec![SignedEntityRecord {
            signed_entity_id: entity.signed_entity_id.clone(),
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(entity.artifact.epoch),
            certificate_id: entity.certificate_id.clone(),
            artifact: serde_json::to_string(&entity.artifact).unwrap(),
            created_at: entity.created_at,
        }];
        let message = ToMithrilStakeDistributionListMessageAdapter::adapt(vec![entity]);
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_last_signed_entities_by_type()
            .return_once(|_, _| Ok(records))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_mithril_stake_distribution_list_message(10)
            .await
            .unwrap();

        assert_eq!(message, response);
    }

    #[tokio::test]
    async fn get_cardano_transaction() {
        let entity = SignedEntity::<CardanoTransactionsSnapshot>::dummy();
        let record = SignedEntityRecord {
            signed_entity_id: entity.signed_entity_id.clone(),
            signed_entity_type: SignedEntityType::CardanoTransactions(
                entity.signed_entity_type.get_epoch(),
                entity.artifact.block_number,
            ),
            certificate_id: entity.certificate_id.clone(),
            artifact: serde_json::to_string(&entity.artifact).unwrap(),
            created_at: entity.created_at,
        };
        let message = ToCardanoTransactionMessageAdapter::adapt(entity);
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(Some(record)))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_cardano_transaction_message("whatever")
            .await
            .unwrap()
            .expect("A CardanoTransactionMessage was expected.");

        assert_eq!(message, response);
    }

    #[tokio::test]
    async fn get_cardano_transaction_not_exist() {
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_signed_entity()
            .return_once(|_| Ok(None))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_cardano_transaction_message("whatever")
            .await
            .unwrap();

        assert!(response.is_none());
    }

    #[tokio::test]
    async fn get_cardano_transaction_list_message() {
        let entity = SignedEntity::<CardanoTransactionsSnapshot>::dummy();
        let records = vec![SignedEntityRecord {
            signed_entity_id: entity.signed_entity_id.clone(),
            signed_entity_type: SignedEntityType::CardanoTransactions(
                entity.signed_entity_type.get_epoch(),
                entity.artifact.block_number,
            ),
            certificate_id: entity.certificate_id.clone(),
            artifact: serde_json::to_string(&entity.artifact).unwrap(),
            created_at: entity.created_at,
        }];
        let message = ToCardanoTransactionListMessageAdapter::adapt(vec![entity]);
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let mut storer = MockSignedEntityStorer::new();
        storer
            .expect_get_last_signed_entities_by_type()
            .return_once(|_, _| Ok(records))
            .once();
        dep_builder.signed_entity_storer = Some(Arc::new(storer));
        let service = dep_builder.get_message_service().await.unwrap();
        let response = service
            .get_cardano_transaction_list_message(10)
            .await
            .unwrap();

        assert_eq!(message, response);
    }
}
