use anyhow::Context;
use async_trait::async_trait;
use chrono::Utc;
use slog::{debug, info, trace, warn, Logger};
use std::sync::Arc;

use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::crypto_helper::{ProtocolGenesisVerifier, PROTOCOL_VERSION};
use mithril_common::entities::{
    Certificate, CertificateMetadata, CertificateSignature, Epoch, ProtocolMessage,
    SignedEntityType, SingleSignature, StakeDistributionParty,
};
use mithril_common::logging::LoggerExtensions;
use mithril_common::protocol::ToMessage;
use mithril_common::{CardanoNetwork, StdResult};

use crate::database::record::{OpenMessageRecord, OpenMessageWithSingleSignaturesRecord};
use crate::database::repository::{
    CertificateRepository, OpenMessageRepository, SingleSignatureRepository,
};
use crate::dependency_injection::EpochServiceWrapper;
use crate::entities::OpenMessage;
use crate::services::{CertifierService, CertifierServiceError, SignatureRegistrationStatus};
use crate::MultiSigner;

/// Mithril CertifierService implementation
pub struct MithrilCertifierService {
    network: CardanoNetwork,
    open_message_repository: Arc<OpenMessageRepository>,
    single_signature_repository: Arc<SingleSignatureRepository>,
    certificate_repository: Arc<CertificateRepository>,
    certificate_verifier: Arc<dyn CertificateVerifier>,
    genesis_verifier: Arc<ProtocolGenesisVerifier>,
    multi_signer: Arc<dyn MultiSigner>,
    epoch_service: EpochServiceWrapper,
    logger: Logger,
}

impl MithrilCertifierService {
    /// instantiate the service
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        network: CardanoNetwork,
        open_message_repository: Arc<OpenMessageRepository>,
        single_signature_repository: Arc<SingleSignatureRepository>,
        certificate_repository: Arc<CertificateRepository>,
        certificate_verifier: Arc<dyn CertificateVerifier>,
        genesis_verifier: Arc<ProtocolGenesisVerifier>,
        multi_signer: Arc<dyn MultiSigner>,
        epoch_service: EpochServiceWrapper,
        logger: Logger,
    ) -> Self {
        Self {
            network,
            open_message_repository,
            single_signature_repository,
            certificate_repository,
            multi_signer,
            certificate_verifier,
            genesis_verifier,
            epoch_service,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn get_open_message_record(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessageWithSingleSignaturesRecord>> {
        debug!(
            self.logger,
            ">> get_open_message_record(signed_entity_type: {signed_entity_type:?})"
        );

        let open_message_with_single_signatures = self
            .open_message_repository
            .get_open_message_with_single_signatures(signed_entity_type)
            .await
            .with_context(|| format!("Certifier can not get open message with single signatures for signed entity type: '{signed_entity_type}'"))?;

        Ok(open_message_with_single_signatures)
    }
}

#[async_trait]
impl CertifierService for MithrilCertifierService {
    async fn inform_epoch(&self, epoch: Epoch) -> StdResult<()> {
        debug!(self.logger, ">> inform_epoch(epoch: {epoch:?})");
        let nb = self
            .open_message_repository
            .clean_epoch(epoch)
            .await
            .with_context(|| {
                format!("Certifier can not clean open messages from epoch '{epoch}'")
            })?;
        info!(self.logger, "Informed of a new Epoch: {epoch:?}. Cleaned {nb} open messages along with their single signatures.");

        Ok(())
    }

    async fn register_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
    ) -> StdResult<SignatureRegistrationStatus> {
        debug!(self.logger, ">> register_single_signature(signed_entity_type: {signed_entity_type:?}, single_signatures: {signature:?}");
        trace!(self.logger, ">> register_single_signature"; "complete_single_signatures" => #?signature);

        let open_message = self
            .get_open_message_record(signed_entity_type)
            .await.with_context(|| format!("CertifierService can not get open message record for signed_entity_type: '{signed_entity_type}'"))?
            .ok_or_else(|| {
                warn!(self.logger, "register_single_signature: OpenMessage not found for type {signed_entity_type:?}.");
                CertifierServiceError::NotFound(signed_entity_type.clone())
            })?;

        if open_message.is_certified {
            warn!(self.logger, "register_single_signature: open message {signed_entity_type:?} is already certified, cannot register single signature.");

            return Err(CertifierServiceError::AlreadyCertified(signed_entity_type.clone()).into());
        }

        if open_message.is_expired {
            warn!(self.logger, "register_single_signature: open message {signed_entity_type:?} has expired, cannot register single signature.");

            return Err(CertifierServiceError::Expired(signed_entity_type.clone()).into());
        }

        self.multi_signer
            .verify_single_signature(&open_message.protocol_message.to_message(), signature)
            .await
            .map_err(|err| {
                CertifierServiceError::InvalidSingleSignature(signed_entity_type.clone(), err)
            })?;

        let single_signature = self
            .single_signature_repository
            .create_single_signature(signature, &open_message.clone().into())
            .await.with_context(|| format!("Certifier can not create the single signature from single_signature: '{signature:?}', open_message: '{open_message:?}'"))?;
        info!(self.logger, "register_single_signature: created pool '{}' single signature for {signed_entity_type:?}.", single_signature.signer_id);
        debug!(
            self.logger,
            "register_single_signature: created single signature for open message ID='{}'.",
            single_signature.open_message_id
        );

        Ok(SignatureRegistrationStatus::Registered)
    }

    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage> {
        debug!(
            self.logger, ">> create_open_message(signed_entity_type: {signed_entity_type:?})";
            "protocol_message" => ?protocol_message
        );
        let open_message = self
            .open_message_repository
            .create_open_message(
                signed_entity_type.get_epoch_when_signed_entity_type_is_signed(),
                signed_entity_type,
                protocol_message,
            )
            .await
            .with_context(|| {
                format!(
                    "Certifier can not create open message from protocol_message: '{:?}, epoch: '{}''",
                    protocol_message,
                    signed_entity_type.get_epoch_when_signed_entity_type_is_signed()
                )
            })?;
        info!(
            self.logger,
            "create_open_message: created open message for {signed_entity_type:?}"
        );
        debug!(
            self.logger,
            "create_open_message: created open message ID='{}'", open_message.open_message_id
        );

        Ok(open_message.into())
    }

    async fn get_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        debug!(
            self.logger,
            ">> get_open_message(signed_entity_type: {signed_entity_type:?})"
        );

        let open_message = self
            .open_message_repository
            .get_open_message_with_single_signatures(signed_entity_type)
            .await
            .with_context(|| format!("Certifier can not get open message with single signatures for signed entity type: '{signed_entity_type}'"))?
            .map(|record| record.into());

        Ok(open_message)
    }

    async fn mark_open_message_if_expired(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        debug!(self.logger, ">> mark_open_message_if_expired");

        let mut open_message_record = self
            .open_message_repository
            .get_expired_open_message(signed_entity_type)
            .await
            .with_context(|| "Certifier can not get expired open messages")?;
        if let Some(open_message_record) = open_message_record.as_mut() {
            open_message_record.is_expired = true;
            self.open_message_repository
                .update_open_message(open_message_record)
                .await
                .with_context(|| "Certifier can not update open message to mark it as expired")?;
        }

        Ok(open_message_record.map(|record| record.into()))
    }

    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<Certificate>> {
        debug!(
            self.logger,
            ">> create_certificate(signed_entity_type: {signed_entity_type:?})"
        );
        let open_message_record = self
            .get_open_message_record(signed_entity_type)
            .await?
            .ok_or_else(|| {
                warn!(
                    self.logger,
                    "create_certificate: OpenMessage not found for type {signed_entity_type:?}."
                );
                CertifierServiceError::NotFound(signed_entity_type.clone())
            })?;
        let open_message: OpenMessage = open_message_record.clone().into();

        if open_message.is_certified {
            warn!(self.logger, "create_certificate: open message {signed_entity_type:?} is already certified, cannot create certificate.");

            return Err(CertifierServiceError::AlreadyCertified(signed_entity_type.clone()).into());
        }

        if open_message.is_expired {
            warn!(self.logger, "create_certificate: open message {signed_entity_type:?} is expired, cannot create certificate.");

            return Err(CertifierServiceError::Expired(signed_entity_type.clone()).into());
        }

        let multi_signature = match self.multi_signer.create_multi_signature(&open_message).await? {
            None => {
                debug!(self.logger, "create_certificate: No multi-signature could be created for open message {signed_entity_type:?}");
                return Ok(None);
            }
            Some(signature) => {
                info!(self.logger, "create_certificate: multi-signature created for open message {signed_entity_type:?}");
                signature
            }
        };

        let epoch_service = self.epoch_service.read().await;
        let signer_ids = open_message.get_signers_id();
        let signers = epoch_service
            .current_signers_with_stake()?
            .clone()
            .into_iter()
            .filter(|signer| signer_ids.contains(&signer.party_id))
            .collect::<Vec<_>>();

        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = open_message.created_at;
        let sealed_at = Utc::now();
        let metadata = CertificateMetadata::new(
            self.network,
            protocol_version,
            epoch_service.current_protocol_parameters()?.clone(),
            initiated_at,
            sealed_at,
            StakeDistributionParty::from_signers(signers),
        );
        let parent_certificate_hash = self
            .certificate_repository
            .get_master_certificate_for_epoch::<Certificate>(open_message.epoch)
            .await
            .with_context(|| {
                format!(
                    "Certifier can not get leader certificate for epoch: '{}'",
                    open_message.epoch
                )
            })?
            .map(|cert| cert.hash)
            .ok_or_else(|| Box::new(CertifierServiceError::NoParentCertificateFound))?;

        let certificate = Certificate::new(
            parent_certificate_hash,
            open_message.epoch,
            metadata,
            open_message.protocol_message.clone(),
            epoch_service.current_aggregate_verification_key()?.clone(),
            CertificateSignature::MultiSignature(signed_entity_type.clone(), multi_signature),
        );

        self.certificate_verifier
            .verify_certificate(&certificate, &self.genesis_verifier.to_verification_key())
            .await
            .with_context(|| {
                format!(
                    "CertificateVerifier can not verify certificate with hash: '{}'",
                    certificate.hash
                )
            })?;

        let certificate = self
            .certificate_repository
            .create_certificate(certificate)
            .await
            .with_context(|| {format!(
                "Certifier can not create certificate for signed entity type: '{signed_entity_type}'")
            })?;

        let mut open_message_certified: OpenMessageRecord = open_message_record.into();
        open_message_certified.is_certified = true;
        self.open_message_repository
            .update_open_message(&open_message_certified)
            .await
            .with_context(|| format!("Certifier can not update open message for signed entity type: '{signed_entity_type}'"))
            ?;

        Ok(Some(certificate))
    }

    async fn get_certificate_by_hash(&self, hash: &str) -> StdResult<Option<Certificate>> {
        self.certificate_repository.get_certificate(hash).await
    }

    async fn get_latest_certificates(&self, last_n: usize) -> StdResult<Vec<Certificate>> {
        self.certificate_repository
            .get_latest_certificates(last_n)
            .await
            .with_context(|| format!("Certifier can not get last '{last_n}' certificates"))
    }

    async fn verify_certificate_chain(&self, epoch: Epoch) -> StdResult<()> {
        if let Some(certificate) = self
            .certificate_repository
            .get_latest_certificates::<Certificate>(1)
            .await?
            .first()
        {
            if epoch.has_gap_with(&certificate.epoch) {
                return Err(CertifierServiceError::CertificateEpochGap {
                    certificate_epoch: certificate.epoch,
                    current_epoch: epoch,
                }
                .into());
            }

            self.certificate_verifier
                .verify_certificate_chain(
                    certificate.to_owned(),
                    &self.genesis_verifier.to_verification_key(),
                )
                .await
                .with_context(|| "CertificateVerifier can not verify certificate chain")?;

            Ok(())
        } else {
            Err(CertifierServiceError::CouldNotFindLastCertificate.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::{
        dependency_injection::DependenciesBuilder, multi_signer::MockMultiSigner,
        services::FakeEpochService, test_tools::TestLogger, ServeCommandConfiguration,
    };
    use chrono::{DateTime, Days};
    use mithril_common::{
        entities::{CardanoDbBeacon, ProtocolMessagePartKey},
        temp_dir,
        test_utils::{fake_data, MithrilFixture, MithrilFixtureBuilder},
    };
    use tokio::sync::RwLock;

    use super::*;

    impl MithrilCertifierService {
        async fn from_deps(
            network: CardanoNetwork,
            mut dependency_builder: DependenciesBuilder,
        ) -> Self {
            let connection = dependency_builder.get_sqlite_connection().await.unwrap();
            let open_message_repository = Arc::new(OpenMessageRepository::new(connection.clone()));
            let single_signature_repository =
                Arc::new(SingleSignatureRepository::new(connection.clone()));
            let certificate_repository = Arc::new(CertificateRepository::new(connection));
            let certificate_verifier = dependency_builder.get_certificate_verifier().await.unwrap();
            let genesis_verifier = dependency_builder.get_genesis_verifier().await.unwrap();
            let multi_signer = dependency_builder.get_multi_signer().await.unwrap();
            let epoch_service = dependency_builder.get_epoch_service().await.unwrap();

            Self::new(
                network,
                open_message_repository,
                single_signature_repository,
                certificate_repository,
                certificate_verifier,
                genesis_verifier,
                multi_signer,
                epoch_service,
                TestLogger::stdout(),
            )
        }
    }

    /// Note: If current_epoch is provided the [EpochService] will be automatically initialized
    async fn setup_certifier_service_with_network(
        snapshot_directory: PathBuf,
        network: CardanoNetwork,
        fixture: &MithrilFixture,
        epochs_with_signers: &[Epoch],
        current_epoch: Option<Epoch>,
    ) -> MithrilCertifierService {
        let configuration = ServeCommandConfiguration::new_sample(snapshot_directory);
        let cardano_transactions_signing_config =
            configuration.cardano_transactions_signing_config.clone();
        let mut dependency_builder =
            DependenciesBuilder::new_with_stdout_logger(Arc::new(configuration));
        if let Some(epoch) = current_epoch {
            dependency_builder.epoch_service = Some(Arc::new(RwLock::new(
                FakeEpochService::from_fixture(epoch, fixture),
            )));
        }

        let dependency_manager =
            dependency_builder.build_serve_dependencies_container().await.unwrap();
        dependency_manager
            .init_state_from_fixture(
                fixture,
                &cardano_transactions_signing_config,
                epochs_with_signers,
            )
            .await;

        MithrilCertifierService::from_deps(network, dependency_builder).await
    }

    async fn setup_certifier_service(
        snapshot_directory: PathBuf,
        fixture: &MithrilFixture,
        epochs_with_signers: &[Epoch],
        current_epoch: Option<Epoch>,
    ) -> MithrilCertifierService {
        setup_certifier_service_with_network(
            snapshot_directory,
            fake_data::network(),
            fixture,
            epochs_with_signers,
            current_epoch,
        )
        .await
    }

    #[tokio::test]
    async fn should_clean_epoch_when_inform_epoch() {
        let beacon = CardanoDbBeacon::new(1, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epoch = beacon.epoch;
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        certifier_service
            .create_open_message(&signed_entity_type, &protocol_message)
            .await
            .unwrap();
        certifier_service.inform_epoch(epoch + 1).await.unwrap();
        let open_message = certifier_service.get_open_message(&signed_entity_type).await.unwrap();
        assert!(open_message.is_none());
    }

    #[tokio::test]
    async fn should_mark_open_message_expired_when_exists() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        let mut open_message = certifier_service
            .open_message_repository
            .create_open_message(beacon.epoch, &signed_entity_type, &protocol_message)
            .await
            .unwrap();
        open_message.expires_at = Some(
            DateTime::parse_from_rfc3339("2000-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        );
        certifier_service
            .open_message_repository
            .update_open_message(&open_message)
            .await
            .unwrap();

        let open_message = certifier_service
            .mark_open_message_if_expired(&signed_entity_type)
            .await
            .expect("mark_open_message_if_expired should not fail");
        assert!(open_message.is_some());
        assert!(open_message.unwrap().is_expired);
    }

    #[tokio::test]
    async fn should_not_mark_open_message_expired_when_does_not_expire() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        let mut open_message = certifier_service
            .open_message_repository
            .create_open_message(beacon.epoch, &signed_entity_type, &protocol_message)
            .await
            .unwrap();
        open_message.expires_at = None;
        certifier_service
            .open_message_repository
            .update_open_message(&open_message)
            .await
            .unwrap();

        let open_message = certifier_service
            .mark_open_message_if_expired(&signed_entity_type)
            .await
            .expect("mark_open_message_if_expired should not fail");
        assert!(open_message.is_none());
    }

    #[tokio::test]
    async fn should_not_mark_open_message_expired_when_has_not_expired_yet() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        let mut open_message = certifier_service
            .open_message_repository
            .create_open_message(beacon.epoch, &signed_entity_type, &protocol_message)
            .await
            .unwrap();
        open_message.expires_at = Some(Utc::now().checked_add_days(Days::new(1)).unwrap());
        certifier_service
            .open_message_repository
            .update_open_message(&open_message)
            .await
            .unwrap();

        let open_message = certifier_service
            .mark_open_message_if_expired(&signed_entity_type)
            .await
            .expect("mark_open_message_if_expired should not fail");
        assert!(open_message.is_none());
    }

    #[tokio::test]
    async fn should_register_valid_single_signature() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=3).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service = setup_certifier_service(
            temp_dir!(),
            &fixture,
            &epochs_with_signers,
            Some(beacon.epoch),
        )
        .await;

        certifier_service
            .create_open_message(&signed_entity_type, &protocol_message)
            .await
            .unwrap();

        let mut signatures = Vec::new();
        for signer_fixture in fixture.signers_fixture() {
            if let Some(signature) = signer_fixture.sign(&protocol_message) {
                signatures.push(signature);
            }
        }
        certifier_service
            .register_single_signature(&signed_entity_type, &signatures[0])
            .await
            .unwrap();
        let open_message = certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .unwrap()
            .unwrap();
        assert!(!open_message.single_signatures.is_empty());
    }

    #[tokio::test]
    async fn should_not_register_invalid_single_signature() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let mut protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service = setup_certifier_service(
            temp_dir!(),
            &fixture,
            &epochs_with_signers,
            Some(beacon.epoch),
        )
        .await;

        certifier_service
            .create_open_message(&signed_entity_type, &protocol_message)
            .await
            .unwrap();

        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "snapshot-digest-123".to_string(),
        );

        let mut signatures = Vec::new();
        for signer_fixture in fixture.signers_fixture() {
            if let Some(signature) = signer_fixture.sign(&protocol_message) {
                signatures.push(signature);
            }
        }
        let err = certifier_service
            .register_single_signature(&signed_entity_type, &signatures[0])
            .await
            .expect_err("register_single_signature should fail");

        assert!(
            matches!(
                err.downcast_ref::<CertifierServiceError>(),
                Some(CertifierServiceError::InvalidSingleSignature(..))
            ),
            "Expected CertifierServiceError::InvalidSingleSignature, got: '{err:?}'"
        );
    }

    #[tokio::test]
    async fn should_not_register_single_signature_for_certified_open_message() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        let mut open_message = certifier_service
            .open_message_repository
            .create_open_message(beacon.epoch, &signed_entity_type, &protocol_message)
            .await
            .unwrap();
        open_message.is_certified = true;
        certifier_service
            .open_message_repository
            .update_open_message(&open_message)
            .await
            .unwrap();

        let mut signatures = Vec::new();
        for signer_fixture in fixture.signers_fixture() {
            if let Some(signature) = signer_fixture.sign(&protocol_message) {
                signatures.push(signature);
            }
        }
        certifier_service
            .register_single_signature(&signed_entity_type, &signatures[0])
            .await
            .expect_err("register_single_signature should fail");
    }

    #[tokio::test]
    async fn should_not_register_single_signature_for_expired_open_message() {
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        let mut open_message = certifier_service
            .open_message_repository
            .create_open_message(beacon.epoch, &signed_entity_type, &protocol_message)
            .await
            .unwrap();
        open_message.is_expired = true;
        certifier_service
            .open_message_repository
            .update_open_message(&open_message)
            .await
            .unwrap();

        let mut signatures = Vec::new();
        for signer_fixture in fixture.signers_fixture() {
            if let Some(signature) = signer_fixture.sign(&protocol_message) {
                signatures.push(signature);
            }
        }
        certifier_service
            .register_single_signature(&signed_entity_type, &signatures[0])
            .await
            .expect_err("register_single_signature should fail");
    }

    #[tokio::test]
    async fn should_create_certificate_when_multi_signature_produced() {
        let network = fake_data::network();
        let beacon = CardanoDbBeacon::new(3, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "3".to_string());
        let epochs_with_signers = (1..=3).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let certifier_service = setup_certifier_service_with_network(
            temp_dir!(),
            network,
            &fixture,
            &epochs_with_signers,
            Some(beacon.epoch),
        )
        .await;

        certifier_service
            .create_open_message(&signed_entity_type, &protocol_message)
            .await
            .unwrap();

        let genesis_certificate = fixture.create_genesis_certificate(network, beacon.epoch - 1);
        certifier_service
            .certificate_repository
            .create_certificate(genesis_certificate)
            .await
            .unwrap();

        let mut signatures = Vec::new();
        for signer_fixture in fixture.signers_fixture() {
            if let Some(signature) = signer_fixture.sign(&protocol_message) {
                signatures.push(signature);
            }
        }
        for signature in signatures {
            certifier_service
                .register_single_signature(&signed_entity_type, &signature)
                .await
                .expect("register_single_signature should not fail");
        }

        let create_certificate_result = certifier_service
            .create_certificate(&signed_entity_type)
            .await
            .unwrap();
        assert!(create_certificate_result.is_some());

        let certificate_created = create_certificate_result.unwrap();
        certifier_service
            .certificate_verifier
            .verify_certificate(
                &certificate_created,
                &certifier_service.genesis_verifier.to_verification_key(),
            )
            .await
            .unwrap();

        let open_message = certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .unwrap()
            .unwrap();
        assert!(open_message.is_certified);

        let certificate_retrieved = certifier_service
            .get_certificate_by_hash(&certificate_created.hash)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(certificate_created, certificate_retrieved);

        let latest_certificates = certifier_service.get_latest_certificates(10).await.unwrap();
        assert!(!latest_certificates.is_empty());
    }

    #[tokio::test]
    async fn should_not_create_certificate_for_open_message_not_created() {
        let beacon = CardanoDbBeacon::new(1, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        certifier_service
            .create_certificate(&signed_entity_type)
            .await
            .expect_err("create_certificate should fail");
    }

    #[tokio::test]
    async fn should_not_create_certificate_for_open_message_already_certified() {
        let beacon = CardanoDbBeacon::new(1, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epoch = beacon.epoch;
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        certifier_service
            .open_message_repository
            .create_open_message(epoch, &signed_entity_type, &protocol_message)
            .await
            .unwrap();
        certifier_service
            .create_certificate(&signed_entity_type)
            .await
            .expect_err("create_certificate should fail");
    }

    #[tokio::test]
    async fn should_not_create_certificate_when_no_multi_signature_produced() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_create_multi_signature()
            .return_once(move |_| Ok(None));
        let beacon = CardanoDbBeacon::new(1, 1);
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon.clone());
        let protocol_message = ProtocolMessage::new();
        let epochs_with_signers = (1..=5).map(Epoch).collect::<Vec<_>>();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let mut certifier_service =
            setup_certifier_service(temp_dir!(), &fixture, &epochs_with_signers, None).await;
        certifier_service.multi_signer = Arc::new(mock_multi_signer);
        certifier_service
            .create_open_message(&signed_entity_type, &protocol_message)
            .await
            .unwrap();
        let create_certificate_result = certifier_service
            .create_certificate(&signed_entity_type)
            .await
            .unwrap();
        assert!(create_certificate_result.is_none());
    }

    #[tokio::test]
    async fn test_epoch_gap_certificate_chain() {
        let builder = MithrilFixtureBuilder::default();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &builder.build(), &[], None).await;
        let certificate = fake_data::genesis_certificate("whatever");
        let epoch = certificate.epoch + 2;
        certifier_service
            .certificate_repository
            .create_certificate(certificate)
            .await
            .unwrap();
        let error = certifier_service.verify_certificate_chain(epoch).await.unwrap_err();

        if let Some(err) = error.downcast_ref::<CertifierServiceError>() {
            assert!(
                matches!(err, CertifierServiceError::CertificateEpochGap {certificate_epoch: _, current_epoch} if *current_epoch == epoch)
            );
        } else {
            panic!("Unexpected error {error:?}");
        }
    }

    #[tokio::test]
    async fn test_epoch_gap_certificate_chain_ok() {
        let builder = MithrilFixtureBuilder::default();
        let certifier_service =
            setup_certifier_service(temp_dir!(), &builder.build(), &[], None).await;
        let certificate = fake_data::genesis_certificate("whatever");
        let epoch = certificate.epoch + 1;
        certifier_service
            .certificate_repository
            .create_certificate(certificate)
            .await
            .unwrap();
        let error = certifier_service.verify_certificate_chain(epoch).await.unwrap_err();

        if let Some(err) = error.downcast_ref::<CertifierServiceError>() {
            assert!(!matches!(
                err,
                CertifierServiceError::CertificateEpochGap {
                    certificate_epoch: _,
                    current_epoch: _
                }
            ));
        }
    }
}
