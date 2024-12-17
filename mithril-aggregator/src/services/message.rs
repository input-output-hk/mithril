//! This service is responsible for providing HTTP server with messages as fast as possible.

use std::sync::Arc;

use async_trait::async_trait;
use thiserror::Error;

use mithril_common::{
    entities::{Epoch, SignedEntityTypeDiscriminants},
    messages::{
        CardanoDatabaseSnapshotListMessage, CardanoDatabaseSnapshotMessage,
        CardanoStakeDistributionListMessage, CardanoStakeDistributionMessage,
        CardanoTransactionSnapshotListMessage, CardanoTransactionSnapshotMessage,
        CertificateListMessage, CertificateMessage, MithrilStakeDistributionListMessage,
        MithrilStakeDistributionMessage, SnapshotListMessage, SnapshotMessage,
    },
    StdResult,
};

use crate::database::repository::{CertificateRepository, SignedEntityStorer};

/// Error related to the [MessageService]
#[derive(Debug, Error)]
pub enum MessageServiceError {
    /// There is no current PendingCertificate
    #[error("There is no current pending certificate.")]
    PendingCertificateDoesNotExist,
}
/// HTTP Message service trait.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait MessageService: Sync + Send {
    /// Return the message representation of a certificate if it exists.
    async fn get_certificate_message(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>>;

    /// Return the message representation of the last N certificates.
    async fn get_certificate_list_message(&self, limit: usize)
        -> StdResult<CertificateListMessage>;

    /// Return the information regarding the given snapshot.
    async fn get_snapshot_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SnapshotMessage>>;

    /// Return the list of the last signed snapshots. The limit of the list is
    /// passed as argument.
    async fn get_snapshot_list_message(&self, limit: usize) -> StdResult<SnapshotListMessage>;

    /// Return the information regarding the Cardano database for the given identifier.
    async fn get_cardano_database_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoDatabaseSnapshotMessage>>;

    /// Return the list of the last Cardano database message.
    async fn get_cardano_database_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoDatabaseSnapshotListMessage>;

    /// Return the information regarding the Mithril stake distribution for the given identifier.
    async fn get_mithril_stake_distribution_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<MithrilStakeDistributionMessage>>;

    /// Return the list of the last Mithril stake distributions message.
    async fn get_mithril_stake_distribution_list_message(
        &self,
        limit: usize,
    ) -> StdResult<MithrilStakeDistributionListMessage>;

    /// Return the information regarding the Cardano transactions set for the given identifier.
    async fn get_cardano_transaction_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoTransactionSnapshotMessage>>;

    /// Return the list of the last Cardano transactions set message.
    async fn get_cardano_transaction_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoTransactionSnapshotListMessage>;

    /// Return the information regarding the Cardano stake distribution for the given identifier.
    async fn get_cardano_stake_distribution_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoStakeDistributionMessage>>;

    /// Return the information regarding the Cardano stake distribution for the given epoch.
    async fn get_cardano_stake_distribution_message_by_epoch(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<CardanoStakeDistributionMessage>>;

    /// Return the list of the last Cardano stake distributions message.
    async fn get_cardano_stake_distribution_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoStakeDistributionListMessage>;
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

    async fn get_cardano_database_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoDatabaseSnapshotMessage>> {
        let signed_entity = self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await?;

        signed_entity.map(|v| v.try_into()).transpose()
    }

    async fn get_cardano_database_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoDatabaseSnapshotListMessage> {
        let signed_entity_type_id = SignedEntityTypeDiscriminants::CardanoDatabase;
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

    async fn get_cardano_stake_distribution_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<CardanoStakeDistributionMessage>> {
        let signed_entity = self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await?;

        signed_entity.map(|v| v.try_into()).transpose()
    }

    async fn get_cardano_stake_distribution_message_by_epoch(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<CardanoStakeDistributionMessage>> {
        let signed_entity = self
            .signed_entity_storer
            .get_cardano_stake_distribution_signed_entity_by_epoch(epoch)
            .await?;

        signed_entity.map(|v| v.try_into()).transpose()
    }

    async fn get_cardano_stake_distribution_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CardanoStakeDistributionListMessage> {
        let signed_entity_type_id = SignedEntityTypeDiscriminants::CardanoStakeDistribution;
        let entities = self
            .signed_entity_storer
            .get_last_signed_entities_by_type(&signed_entity_type_id, limit)
            .await?;

        entities.into_iter().map(|i| i.try_into()).collect()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, Certificate, SignedEntityType};
    use mithril_common::test_utils::fake_data;

    use crate::database::record::SignedEntityRecord;
    use crate::database::repository::SignedEntityStore;
    use crate::database::test_helper::main_db_connection;

    use super::*;

    struct MessageServiceBuilder {
        certificates: Vec<Certificate>,
        signed_entity_records: Vec<SignedEntityRecord>,
    }

    impl MessageServiceBuilder {
        fn new() -> Self {
            Self {
                certificates: Vec::new(),
                signed_entity_records: Vec::new(),
            }
        }

        fn with_certificates(mut self, certificates: &[Certificate]) -> Self {
            self.certificates.extend_from_slice(certificates);
            self
        }

        fn with_signed_entity_records(
            mut self,
            signed_entity_record: &[SignedEntityRecord],
        ) -> Self {
            self.signed_entity_records
                .extend_from_slice(signed_entity_record);
            self
        }

        async fn build(self) -> MithrilMessageService {
            let connection = Arc::new(main_db_connection().unwrap());
            let certificate_repository = CertificateRepository::new(connection.clone());
            let signed_entity_store = SignedEntityStore::new(connection);

            certificate_repository
                .create_many_certificates(self.certificates)
                .await
                .unwrap();
            for record in self.signed_entity_records {
                signed_entity_store
                    .store_signed_entity(&record)
                    .await
                    .unwrap();
            }

            MithrilMessageService::new(
                Arc::new(certificate_repository),
                Arc::new(signed_entity_store),
            )
        }
    }

    mod certificate {
        use super::*;

        #[tokio::test]
        async fn get_no_certificate() {
            let service = MessageServiceBuilder::new().build().await;

            let certificate_hash = "whatever";
            let certificate_message = service
                .get_certificate_message(certificate_hash)
                .await
                .unwrap();
            assert!(certificate_message.is_none());
        }

        #[tokio::test]
        async fn get_certificate() {
            let genesis_certificate = fake_data::genesis_certificate("genesis_hash");
            let service = MessageServiceBuilder::new()
                .with_certificates(&[genesis_certificate.clone()])
                .build()
                .await;

            let certificate_message = service
                .get_certificate_message(&genesis_certificate.hash)
                .await
                .unwrap()
                .expect("There should be a certificate.");
            assert_eq!(genesis_certificate.hash, certificate_message.hash);
        }

        #[tokio::test]
        async fn get_last_certificates() {
            let certificates = [
                fake_data::genesis_certificate("certificate_1"),
                fake_data::genesis_certificate("certificate_2"),
            ];
            let last_certificate_hash = certificates[1].hash.clone();
            let service = MessageServiceBuilder::new()
                .with_certificates(&certificates)
                .build()
                .await;

            let certificate_messages = service.get_certificate_list_message(5).await.unwrap();

            assert_eq!(2, certificate_messages.len());
            assert_eq!(last_certificate_hash, certificate_messages[0].hash);
        }
    }

    mod snapshot {
        use super::*;

        #[tokio::test]
        async fn get_snapshot_not_exist() {
            let service = MessageServiceBuilder::new().build().await;
            let snapshot = service.get_snapshot_message("whatever").await.unwrap();

            assert!(snapshot.is_none());
        }

        #[tokio::test]
        async fn get_snapshot() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(fake_data::beacon()),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::snapshots(1)[0]).unwrap(),
                created_at: Default::default(),
            };
            let message: SnapshotMessage = record.clone().try_into().unwrap();

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record.clone()])
                .build()
                .await;

            let response = service
                .get_snapshot_message(&record.signed_entity_id)
                .await
                .unwrap()
                .expect("A SnapshotMessage was expected.");

            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_snapshot_list_message() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(fake_data::beacon()),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::snapshots(1)[0]).unwrap(),
                created_at: Default::default(),
            };
            let message: SnapshotListMessage = vec![record.clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record])
                .build()
                .await;

            let response = service.get_snapshot_list_message(3).await.unwrap();

            assert_eq!(message, response);
        }
    }

    mod cardano_database {
        use super::*;

        #[tokio::test]
        async fn get_cardano_database_not_exist() {
            let service = MessageServiceBuilder::new().build().await;
            let snapshot = service.get_snapshot_message("whatever").await.unwrap();

            assert!(snapshot.is_none());
        }

        #[tokio::test]
        async fn get_cardano_database() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoDatabaseSnapshotMessage = record.clone().try_into().unwrap();

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record.clone()])
                .build()
                .await;

            let response = service
                .get_cardano_database_message(&record.signed_entity_id)
                .await
                .unwrap()
                .expect("A CardanoDatabaseSnapshotMessage was expected.");

            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_cardano_database_list_message() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoDatabaseSnapshotListMessage =
                vec![record.clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record])
                .build()
                .await;

            let response = service.get_cardano_database_list_message(3).await.unwrap();

            assert_eq!(message, response);
        }
    }

    mod mithril_stake_distribution {
        use super::*;

        #[tokio::test]
        async fn get_mithril_stake_distribution() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(18)),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::mithril_stake_distributions(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: MithrilStakeDistributionMessage = record.clone().try_into().unwrap();

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record.clone()])
                .build()
                .await;

            let response = service
                .get_mithril_stake_distribution_message(&record.signed_entity_id)
                .await
                .unwrap()
                .expect("A MithrilStakeDistributionMessage was expected.");

            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_mithril_stake_distribution_not_exist() {
            let service = MessageServiceBuilder::new().build().await;

            let response = service
                .get_mithril_stake_distribution_message("whatever")
                .await
                .unwrap();

            assert!(response.is_none());
        }

        #[tokio::test]
        async fn get_mithril_stake_distribution_list_message() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(18)),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::mithril_stake_distributions(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: MithrilStakeDistributionListMessage =
                vec![record.clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record])
                .build()
                .await;

            let response = service
                .get_mithril_stake_distribution_list_message(10)
                .await
                .unwrap();

            assert_eq!(message, response);
        }
    }

    mod cardano_transaction {
        use super::*;

        #[tokio::test]
        async fn get_cardano_transaction() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoTransactions(
                    Epoch(18),
                    BlockNumber(120),
                ),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_transactions_snapshot(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoTransactionSnapshotMessage = record.clone().try_into().unwrap();

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record.clone()])
                .build()
                .await;

            let response = service
                .get_cardano_transaction_message(&record.signed_entity_id)
                .await
                .unwrap()
                .expect("A CardanoTransactionMessage was expected.");

            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_cardano_transaction_not_exist() {
            let service = MessageServiceBuilder::new().build().await;

            let response = service
                .get_cardano_transaction_message("whatever")
                .await
                .unwrap();

            assert!(response.is_none());
        }

        #[tokio::test]
        async fn get_cardano_transaction_list_message() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoTransactions(
                    Epoch(18),
                    BlockNumber(120),
                ),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_transactions_snapshot(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoTransactionSnapshotListMessage =
                vec![record.clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record])
                .build()
                .await;

            let response = service
                .get_cardano_transaction_list_message(10)
                .await
                .unwrap();

            assert_eq!(message, response);
        }
    }

    mod cardano_stake_distribution {
        use super::*;

        #[tokio::test]
        async fn get_cardano_stake_distribution() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoStakeDistribution(Epoch(18)),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_stake_distributions(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoStakeDistributionMessage = record.clone().try_into().unwrap();

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record.clone()])
                .build()
                .await;

            let response = service
                .get_cardano_stake_distribution_message(&record.signed_entity_id)
                .await
                .unwrap()
                .expect("A CardanoStakeDistributionMessage was expected.");

            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_cardano_stake_distribution_not_exist() {
            let service = MessageServiceBuilder::new().build().await;

            let response = service
                .get_cardano_stake_distribution_message("whatever")
                .await
                .unwrap();

            assert!(response.is_none());
        }

        #[tokio::test]
        async fn get_cardano_stake_distribution_by_epoch() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoStakeDistribution(Epoch(18)),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_stake_distributions(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoStakeDistributionMessage = record.clone().try_into().unwrap();

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record.clone()])
                .build()
                .await;

            let response = service
                .get_cardano_stake_distribution_message_by_epoch(
                    record.signed_entity_type.get_epoch(),
                )
                .await
                .unwrap()
                .expect("A CardanoStakeDistributionMessage was expected.");

            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_cardano_stake_distribution_by_epoch_not_exist() {
            let service = MessageServiceBuilder::new().build().await;

            let response = service
                .get_cardano_stake_distribution_message_by_epoch(Epoch(999))
                .await
                .unwrap();

            assert!(response.is_none());
        }

        #[tokio::test]
        async fn get_cardano_stake_distribution_list_message() {
            let record = SignedEntityRecord {
                signed_entity_id: "signed_entity_id".to_string(),
                signed_entity_type: SignedEntityType::CardanoStakeDistribution(Epoch(18)),
                certificate_id: "cert_id".to_string(),
                artifact: serde_json::to_string(&fake_data::cardano_stake_distributions(1)[0])
                    .unwrap(),
                created_at: Default::default(),
            };
            let message: CardanoStakeDistributionListMessage =
                vec![record.clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&[record])
                .build()
                .await;

            let response = service
                .get_cardano_stake_distribution_list_message(10)
                .await
                .unwrap();

            assert_eq!(message, response);
        }
    }
}
