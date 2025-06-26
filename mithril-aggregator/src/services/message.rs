//! This service is responsible for providing HTTP server with messages as fast as possible.

use std::collections::BTreeSet;
use std::sync::Arc;

use async_trait::async_trait;

use mithril_common::{
    entities::{Epoch, SignedEntityTypeDiscriminants},
    messages::{
        CardanoDatabaseDigestListItemMessage, CardanoDatabaseDigestListMessage,
        CardanoDatabaseSnapshotListMessage, CardanoDatabaseSnapshotMessage,
        CardanoStakeDistributionListMessage, CardanoStakeDistributionMessage,
        CardanoTransactionSnapshotListMessage, CardanoTransactionSnapshotMessage,
        CertificateListMessage, CertificateMessage, EpochSettingsMessage,
        MithrilStakeDistributionListMessage, MithrilStakeDistributionMessage, SignerMessagePart,
        SnapshotListMessage, SnapshotMessage,
    },
    StdResult,
};

use crate::{
    database::repository::{CertificateRepository, SignedEntityStorer},
    dependency_injection::EpochServiceWrapper,
    ImmutableFileDigestMapper,
};

/// HTTP Message service trait.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait MessageService: Sync + Send {
    /// Return the epoch settings message if it exists.
    async fn get_epoch_settings_message(
        &self,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> StdResult<EpochSettingsMessage>;

    /// Return the message representation of a certificate if it exists.
    async fn get_certificate_message(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>>;

    /// Return the message representation of the latest genesis certificate.
    async fn get_latest_genesis_certificate_message(&self)
        -> StdResult<Option<CertificateMessage>>;

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

    /// Return the list of the Cardano database immutable file names and their digests.
    async fn get_cardano_database_digest_list_message(
        &self,
    ) -> StdResult<CardanoDatabaseDigestListMessage>;

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
    immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,
    epoch_service: EpochServiceWrapper,
}

impl MithrilMessageService {
    /// Constructor
    pub fn new(
        certificate_repository: Arc<CertificateRepository>,
        signed_entity_storer: Arc<dyn SignedEntityStorer>,
        immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,
        epoch_service: EpochServiceWrapper,
    ) -> Self {
        Self {
            certificate_repository,
            signed_entity_storer,
            immutable_file_digest_mapper,
            epoch_service,
        }
    }
}

#[async_trait]
impl MessageService for MithrilMessageService {
    async fn get_epoch_settings_message(
        &self,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> StdResult<EpochSettingsMessage> {
        let epoch_service = self.epoch_service.read().await;

        let epoch = epoch_service.epoch_of_current_data()?;
        let signer_registration_protocol_parameters =
            epoch_service.signer_registration_protocol_parameters()?.clone();
        let current_signers = epoch_service.current_signers()?;
        let next_signers = epoch_service.next_signers()?;

        let cardano_transactions_discriminant =
            allowed_discriminants.get(&SignedEntityTypeDiscriminants::CardanoTransactions);

        let cardano_transactions_signing_config = cardano_transactions_discriminant
            .map(|_| epoch_service.current_cardano_transactions_signing_config())
            .transpose()?
            .cloned();
        let next_cardano_transactions_signing_config = cardano_transactions_discriminant
            .map(|_| epoch_service.next_cardano_transactions_signing_config())
            .transpose()?
            .cloned();

        let epoch_settings_message = EpochSettingsMessage {
            epoch,
            signer_registration_protocol_parameters,
            current_signers: SignerMessagePart::from_signers(current_signers.to_vec()),
            next_signers: SignerMessagePart::from_signers(next_signers.to_vec()),
            cardano_transactions_signing_config,
            next_cardano_transactions_signing_config,
        };

        Ok(epoch_settings_message)
    }

    async fn get_certificate_message(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>> {
        self.certificate_repository.get_certificate(certificate_hash).await
    }

    async fn get_latest_genesis_certificate_message(
        &self,
    ) -> StdResult<Option<CertificateMessage>> {
        self.certificate_repository.get_latest_genesis_certificate().await
    }

    async fn get_certificate_list_message(
        &self,
        limit: usize,
    ) -> StdResult<CertificateListMessage> {
        self.certificate_repository.get_latest_certificates(limit).await
    }

    async fn get_snapshot_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SnapshotMessage>> {
        let signed_entity = self.signed_entity_storer.get_signed_entity(signed_entity_id).await?;

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
        let signed_entity = self.signed_entity_storer.get_signed_entity(signed_entity_id).await?;

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

    async fn get_cardano_database_digest_list_message(
        &self,
    ) -> StdResult<CardanoDatabaseDigestListMessage> {
        Ok(self
            .immutable_file_digest_mapper
            .get_immutable_file_digest_map()
            .await?
            .into_iter()
            .map(
                |(immutable_file_name, digest)| CardanoDatabaseDigestListItemMessage {
                    immutable_file_name,
                    digest,
                },
            )
            .collect::<Vec<_>>())
    }

    async fn get_mithril_stake_distribution_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<MithrilStakeDistributionMessage>> {
        let signed_entity = self.signed_entity_storer.get_signed_entity(signed_entity_id).await?;

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
        let signed_entity = self.signed_entity_storer.get_signed_entity(signed_entity_id).await?;

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
        let signed_entity = self.signed_entity_storer.get_signed_entity(signed_entity_id).await?;

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
    use tokio::sync::RwLock;

    use crate::database::record::SignedEntityRecord;
    use crate::database::repository::{ImmutableFileDigestRepository, SignedEntityStore};
    use crate::database::test_helper::main_db_connection;
    use crate::services::FakeEpochService;

    use super::*;

    struct MessageServiceBuilder {
        certificates: Vec<Certificate>,
        signed_entity_records: Vec<SignedEntityRecord>,
        immutable_file_digest_messages: Vec<CardanoDatabaseDigestListItemMessage>,
        epoch_service: Option<FakeEpochService>,
    }

    impl MessageServiceBuilder {
        fn new() -> Self {
            Self {
                certificates: Vec::new(),
                signed_entity_records: Vec::new(),
                immutable_file_digest_messages: Vec::new(),
                epoch_service: None,
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
            self.signed_entity_records.extend_from_slice(signed_entity_record);

            self
        }

        fn with_immutable_file_digest_messages(
            mut self,
            digests: &[CardanoDatabaseDigestListItemMessage],
        ) -> Self {
            self.immutable_file_digest_messages.extend_from_slice(digests);

            self
        }

        fn with_epoch_service(mut self, epoch_service: FakeEpochService) -> Self {
            self.epoch_service = Some(epoch_service);

            self
        }

        async fn build(self) -> MithrilMessageService {
            let connection = Arc::new(main_db_connection().unwrap());
            let certificate_repository = CertificateRepository::new(connection.clone());
            let signed_entity_store = SignedEntityStore::new(connection.clone());
            let immutable_file_digest_mapper =
                ImmutableFileDigestRepository::new(connection.clone());
            let epoch_service = self.epoch_service.unwrap_or(FakeEpochService::without_data());

            certificate_repository
                .create_many_certificates(self.certificates)
                .await
                .unwrap();
            for record in self.signed_entity_records {
                signed_entity_store.store_signed_entity(&record).await.unwrap();
            }

            for digest_message in self.immutable_file_digest_messages {
                immutable_file_digest_mapper
                    .upsert_immutable_file_digest(
                        &digest_message.immutable_file_name,
                        &digest_message.digest,
                    )
                    .await
                    .unwrap();
            }

            MithrilMessageService::new(
                Arc::new(certificate_repository),
                Arc::new(signed_entity_store),
                Arc::new(immutable_file_digest_mapper),
                Arc::new(RwLock::new(epoch_service)),
            )
        }
    }

    mod epoch_settings {
        use mithril_common::{
            entities::{CardanoTransactionsSigningConfig, ProtocolParameters},
            test_utils::MithrilFixtureBuilder,
        };

        use crate::{entities::AggregatorEpochSettings, services::FakeEpochServiceBuilder};

        use super::*;

        #[tokio::test]
        async fn get_epoch_settings_message() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let epoch_service = FakeEpochService::from_fixture(Epoch(4), &fixture);
            let message_service = MessageServiceBuilder::new()
                .with_epoch_service(epoch_service)
                .build()
                .await;

            let message = message_service
                .get_epoch_settings_message(SignedEntityTypeDiscriminants::all())
                .await
                .unwrap();

            assert_eq!(message.epoch, Epoch(4));
            assert_eq!(
                message.signer_registration_protocol_parameters,
                ProtocolParameters::new(5, 100, 0.65)
            );
            assert_eq!(message.current_signers.len(), 3);
            assert_eq!(message.next_signers.len(), 3);
            assert_eq!(
                message.cardano_transactions_signing_config,
                Some(CardanoTransactionsSigningConfig::new(
                    BlockNumber(0),
                    BlockNumber(15)
                ))
            );
            assert_eq!(
                message.next_cardano_transactions_signing_config,
                Some(CardanoTransactionsSigningConfig::new(
                    BlockNumber(0),
                    BlockNumber(15)
                ))
            );
        }

        #[tokio::test]
        async fn get_epoch_settings_message_with_cardano_transactions_enabled() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let epoch_service = FakeEpochService::from_fixture(Epoch(4), &fixture);
            let message_service = MessageServiceBuilder::new()
                .with_epoch_service(epoch_service)
                .build()
                .await;

            let message = message_service
                .get_epoch_settings_message(BTreeSet::from([
                    SignedEntityTypeDiscriminants::CardanoTransactions,
                ]))
                .await
                .unwrap();

            assert!(message.cardano_transactions_signing_config.is_some());
            assert!(message.next_cardano_transactions_signing_config.is_some(),);
        }

        #[tokio::test]
        async fn get_epoch_settings_message_with_cardano_transactions_not_enabled() {
            let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
            let epoch_service = FakeEpochService::from_fixture(Epoch(4), &fixture);
            let message_service = MessageServiceBuilder::new()
                .with_epoch_service(epoch_service)
                .build()
                .await;

            let message = message_service
                .get_epoch_settings_message(BTreeSet::new())
                .await
                .unwrap();

            assert_eq!(message.cardano_transactions_signing_config, None);
            assert_eq!(message.next_cardano_transactions_signing_config, None);
        }

        #[tokio::test]
        async fn get_epoch_settings_message_retrieves_protocol_parameters_from_epoch_service() {
            let current_epoch_settings = AggregatorEpochSettings {
                protocol_parameters: ProtocolParameters::new(101, 10, 0.5),
                ..AggregatorEpochSettings::dummy()
            };
            let next_epoch_settings = AggregatorEpochSettings {
                protocol_parameters: ProtocolParameters::new(102, 20, 0.5),
                ..AggregatorEpochSettings::dummy()
            };
            let signer_registration_epoch_settings = AggregatorEpochSettings {
                protocol_parameters: ProtocolParameters::new(103, 30, 0.5),
                ..AggregatorEpochSettings::dummy()
            };
            let epoch_service = FakeEpochServiceBuilder {
                current_epoch_settings,
                next_epoch_settings: next_epoch_settings.clone(),
                signer_registration_epoch_settings: signer_registration_epoch_settings.clone(),
                current_signers_with_stake: fake_data::signers_with_stakes(5),
                next_signers_with_stake: fake_data::signers_with_stakes(3),
                ..FakeEpochServiceBuilder::dummy(Epoch(1))
            }
            .build();
            let message_service = MessageServiceBuilder::new()
                .with_epoch_service(epoch_service)
                .build()
                .await;

            let message = message_service
                .get_epoch_settings_message(SignedEntityTypeDiscriminants::all())
                .await
                .unwrap();

            assert_eq!(
                message.signer_registration_protocol_parameters,
                signer_registration_epoch_settings.protocol_parameters
            );
        }

        #[tokio::test]
        async fn get_epoch_settings_message_retrieves_signing_configuration_from_epoch_service() {
            let current_epoch_settings = AggregatorEpochSettings {
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::new(
                    BlockNumber(100),
                    BlockNumber(15),
                ),
                ..AggregatorEpochSettings::dummy()
            };
            let next_epoch_settings = AggregatorEpochSettings {
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::new(
                    BlockNumber(200),
                    BlockNumber(15),
                ),
                ..AggregatorEpochSettings::dummy()
            };
            let epoch_service = FakeEpochServiceBuilder {
                current_epoch_settings: current_epoch_settings.clone(),
                next_epoch_settings: next_epoch_settings.clone(),
                signer_registration_epoch_settings: AggregatorEpochSettings::dummy(),
                current_signers_with_stake: fake_data::signers_with_stakes(5),
                next_signers_with_stake: fake_data::signers_with_stakes(3),
                ..FakeEpochServiceBuilder::dummy(Epoch(1))
            }
            .build();
            let message_service = MessageServiceBuilder::new()
                .with_epoch_service(epoch_service)
                .build()
                .await;

            let message = message_service
                .get_epoch_settings_message(SignedEntityTypeDiscriminants::all())
                .await
                .unwrap();

            assert_eq!(
                message.cardano_transactions_signing_config,
                Some(current_epoch_settings.cardano_transactions_signing_config),
            );
            assert_eq!(
                message.next_cardano_transactions_signing_config,
                Some(next_epoch_settings.cardano_transactions_signing_config),
            );
        }
    }

    mod certificate {
        use super::*;

        #[tokio::test]
        async fn get_no_certificate() {
            let service = MessageServiceBuilder::new().build().await;

            let certificate_hash = "whatever";
            let certificate_message =
                service.get_certificate_message(certificate_hash).await.unwrap();
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
        async fn get_no_latest_genesis_certificate() {
            let service = MessageServiceBuilder::new().build().await;

            let certificate_message =
                service.get_latest_genesis_certificate_message().await.unwrap();
            assert_eq!(None, certificate_message);
        }

        #[tokio::test]
        async fn get_latest_genesis_certificate() {
            let certificates = [
                fake_data::genesis_certificate("certificate_1"),
                fake_data::genesis_certificate("certificate_2"),
                fake_data::certificate("certificate_3"),
            ];
            let last_genesis_hash = certificates[1].hash.clone();
            let service = MessageServiceBuilder::new()
                .with_certificates(&certificates)
                .build()
                .await;

            let certificate_message = service
                .get_latest_genesis_certificate_message()
                .await
                .unwrap()
                .expect("There should be a genesis certificate.");
            assert_eq!(last_genesis_hash, certificate_message.hash);
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
            let records = vec![
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-1".to_string(),
                    signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                        fake_data::beacon(),
                    ),
                    certificate_id: "cert_id-1".to_string(),
                    artifact: serde_json::to_string(&fake_data::snapshots(1)[0]).unwrap(),
                    created_at: Default::default(),
                },
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-2".to_string(),
                    signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                    certificate_id: "cert_id-2".to_string(),
                    artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
            ];
            let message: SnapshotListMessage = vec![records[0].clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&records)
                .build()
                .await;

            let response = service.get_snapshot_list_message(0).await.unwrap();
            assert!(response.is_empty());

            let response = service.get_snapshot_list_message(3).await.unwrap();
            assert_eq!(message, response);
        }
    }

    mod cardano_database {
        use super::*;

        #[tokio::test]
        async fn get_cardano_database_when_record_does_not_exist() {
            let service = MessageServiceBuilder::new().build().await;
            let snapshot = service.get_cardano_database_message("whatever").await.unwrap();

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
            let records = vec![
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-1".to_string(),
                    signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                    certificate_id: "cert_id-1".to_string(),
                    artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-2".to_string(),
                    signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                        fake_data::beacon(),
                    ),
                    certificate_id: "cert_id-2".to_string(),
                    artifact: serde_json::to_string(&fake_data::snapshots(1)[0]).unwrap(),
                    created_at: Default::default(),
                },
            ];
            let message: CardanoDatabaseSnapshotListMessage =
                vec![records[0].clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&records)
                .build()
                .await;

            let response = service.get_cardano_database_list_message(0).await.unwrap();
            assert!(response.is_empty());

            let response = service.get_cardano_database_list_message(3).await.unwrap();
            assert_eq!(message, response);
        }

        #[tokio::test]
        async fn get_cardano_database_digest_list_message() {
            let messages: CardanoDatabaseDigestListMessage = vec![
                CardanoDatabaseDigestListItemMessage {
                    immutable_file_name: "06685.chunk".to_string(),
                    digest: "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e"
                        .to_string(),
                },
                CardanoDatabaseDigestListItemMessage {
                    immutable_file_name: "06685.primary".to_string(),
                    digest: "32dfd6b722d87f253e78eb8b478fb94f1e13463826e674d6ec7b6bf0892b2e39"
                        .to_string(),
                },
            ];

            let service = MessageServiceBuilder::new()
                .with_immutable_file_digest_messages(&messages)
                .build()
                .await;

            let response = service.get_cardano_database_digest_list_message().await.unwrap();

            assert_eq!(messages, response);
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
            let records = vec![
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-1".to_string(),
                    signed_entity_type: SignedEntityType::MithrilStakeDistribution(Epoch(18)),
                    certificate_id: "cert_id-1".to_string(),
                    artifact: serde_json::to_string(&fake_data::mithril_stake_distributions(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-2".to_string(),
                    signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                    certificate_id: "cert_id-2".to_string(),
                    artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
            ];
            let message: MithrilStakeDistributionListMessage =
                vec![records[0].clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&records)
                .build()
                .await;

            let response = service.get_mithril_stake_distribution_list_message(0).await.unwrap();
            assert!(response.is_empty());

            let response = service.get_mithril_stake_distribution_list_message(3).await.unwrap();
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

            let response = service.get_cardano_transaction_message("whatever").await.unwrap();

            assert!(response.is_none());
        }

        #[tokio::test]
        async fn get_cardano_transaction_list_message() {
            let records = vec![
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-1".to_string(),
                    signed_entity_type: SignedEntityType::CardanoTransactions(
                        Epoch(18),
                        BlockNumber(120),
                    ),
                    certificate_id: "cert_id-1".to_string(),
                    artifact: serde_json::to_string(
                        &fake_data::cardano_transactions_snapshot(1)[0],
                    )
                    .unwrap(),
                    created_at: Default::default(),
                },
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-2".to_string(),
                    signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                    certificate_id: "cert_id-2".to_string(),
                    artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
            ];
            let message: CardanoTransactionSnapshotListMessage =
                vec![records[0].clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&records)
                .build()
                .await;

            let response = service.get_cardano_transaction_list_message(0).await.unwrap();
            assert!(response.is_empty());

            let response = service.get_cardano_transaction_list_message(3).await.unwrap();
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
            let records = vec![
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-1".to_string(),
                    signed_entity_type: SignedEntityType::CardanoStakeDistribution(Epoch(18)),
                    certificate_id: "cert_id-1".to_string(),
                    artifact: serde_json::to_string(&fake_data::cardano_stake_distributions(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
                SignedEntityRecord {
                    signed_entity_id: "signed_entity_id-2".to_string(),
                    signed_entity_type: SignedEntityType::CardanoDatabase(fake_data::beacon()),
                    certificate_id: "cert_id-2".to_string(),
                    artifact: serde_json::to_string(&fake_data::cardano_database_snapshots(1)[0])
                        .unwrap(),
                    created_at: Default::default(),
                },
            ];
            let message: CardanoStakeDistributionListMessage =
                vec![records[0].clone().try_into().unwrap()];

            let service = MessageServiceBuilder::new()
                .with_signed_entity_records(&records)
                .build()
                .await;

            let response = service.get_cardano_stake_distribution_list_message(0).await.unwrap();
            assert!(response.is_empty());

            let response = service.get_cardano_stake_distribution_list_message(3).await.unwrap();
            assert_eq!(message, response);
        }
    }
}
