use std::sync::Arc;

use async_trait::async_trait;
use chrono::Utc;
use mithril_common::crypto_helper::{key_encode_hex, PROTOCOL_VERSION};
use mithril_common::entities::{
    Certificate, CertificateMetadata, Epoch, ProtocolMessage, SignedEntityType, SingleSignatures,
};
use mithril_common::StdResult;
use slog::Logger;
use slog_scope::{debug, error, info, warn};
use thiserror::Error;
use tokio::sync::RwLock;

use crate::certificate_creator::CertificateCreationError;
use crate::database::provider::{
    CertificateRepository, OpenMessage, OpenMessageRepository, OpenMessageWithSingleSignatures,
    SingleSignatureRepository,
};
use crate::MultiSigner;

/// Errors dedicated to the CertifierService.
#[derive(Debug, Error)]
pub enum CertifierServiceError {
    /// OpenMessage not found.
    #[error("The open message was not found for beacon {0:?}.")]
    NotFound(SignedEntityType),

    /// The open message is already certified, no more single signatures may be
    /// attached to it nor be certified again.
    #[error("Open message for beacon {0:?} already certified.")]
    AlreadyCertified(SignedEntityType),

    /// The given beacon is older than the current open message for this type.
    #[error("Given beacon {0:?} is older than the current open message beacon.")]
    BeaconTooOld(SignedEntityType),

    /// The given OpenMessage already exists
    #[error("An open message already exist for this beacon {0:?}, cannot create another one.")]
    OpenMessageAlreadyExists(SignedEntityType),

    /// No parent certificate could be found, this certifier cannot create genesis certificates.
    #[error(
        "No parent certificate could be found, this certifier cannot create genesis certificates."
    )]
    NoParentCertificateFound,
}

/// ## CertifierService
///
/// This service manages the open message and their beacon transitions. It can
/// ultimately transform open messages into certificates.
#[async_trait]
pub trait CertifierService: Sync + Send {
    /// Inform the certifier I have detected a new epoch, it may clear its state
    /// and prepare the new signature round. If the given Epoch is equal or less
    /// than the previous informed Epoch, nothing is done.
    async fn inform_epoch(&self, epoch: Epoch) -> StdResult<()>;

    /// Add a new single signature for the open message at the given beacon. If
    /// the open message does not exist or the open message has been certified
    /// since then, an error is returned.
    async fn register_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignatures,
    ) -> StdResult<()>;

    /// Create an open message at the given beacon. If the open message does not
    /// exist or exists at an older beacon, the older open messages are cleared
    /// along with their associated single signatures and the new open message
    /// is created. If the message already exists, an error is returned.
    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage>;

    /// Return the open message at the given Beacon. If the message does not
    /// exist, None is returned.
    async fn get_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessageWithSingleSignatures>>;

    /// Create a certificate if possible. If the pointed open message does
    /// not exist or has been already certified, an error is raised. If a multi
    /// signature is created then the flag `is_certified` of the open
    /// message is set to true. The Certificate is created.
    /// If the stake quorum of the single signatures is
    /// not reached for the multisignature to be created, the certificate is not
    /// created and None is returned. If the certificate can be created, the
    /// list of the registered signers for the given epoch is used.
    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<Certificate>>;
}

/// Mithril CertifierService implementation
pub struct MithrilCertifierService {
    open_message_repository: Arc<OpenMessageRepository>,
    single_signature_repository: Arc<SingleSignatureRepository>,
    certificate_repository: Arc<CertificateRepository>,
    multi_signer: Arc<RwLock<dyn MultiSigner>>,
    current_epoch: Arc<RwLock<Epoch>>,
    _logger: Logger,
}

impl MithrilCertifierService {
    /// instanciate the service
    pub fn new(
        open_message_repository: Arc<OpenMessageRepository>,
        single_signature_repository: Arc<SingleSignatureRepository>,
        certificate_repository: Arc<CertificateRepository>,
        multi_signer: Arc<RwLock<dyn MultiSigner>>,
        current_epoch: Epoch,
        logger: Logger,
    ) -> Self {
        Self {
            open_message_repository,
            single_signature_repository,
            certificate_repository,
            multi_signer,
            current_epoch: Arc::new(RwLock::new(current_epoch)),
            _logger: logger,
        }
    }
}

#[async_trait]
impl CertifierService for MithrilCertifierService {
    async fn inform_epoch(&self, epoch: Epoch) -> StdResult<()> {
        debug!("CertifierService::inform_epoch(epoch: {epoch:?})");
        let mut current_epoch = self.current_epoch.write().await;

        if epoch <= *current_epoch {
            debug!("CertifierService::inform_epoch: given epoch ({epoch:?}) is older than current epoch ({}), ignoring", *current_epoch);

            return Ok(());
        }
        let nb = self
            .open_message_repository
            .clean_epoch(*current_epoch)
            .await?;
        info!("MithrilCertifierService: Got a new Epoch: {epoch:?}. Cleaned {nb} open messages along with their single signatures.");
        *current_epoch = epoch;

        Ok(())
    }

    async fn register_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignatures,
    ) -> StdResult<()> {
        debug!("CertifierService::register_single_signature(signed_entity_type: {signed_entity_type:?}, single_signatures: {signature:?}");
        let open_message = self
            .get_open_message(signed_entity_type)
            .await?
            .ok_or_else(|| {
                warn!("CertifierService::register_single_signature: OpenMessage not found for type {signed_entity_type:?}.");
                CertifierServiceError::NotFound(signed_entity_type.clone())
            })?;

        if open_message.is_certified {
            warn!("CertifierService::register_single_signature: open message {signed_entity_type:?} is already certified, cannot register single signature.");

            return Err(CertifierServiceError::AlreadyCertified(signed_entity_type.clone()).into());
        }
        let single_signature = self
            .single_signature_repository
            .create_single_signature(signature, &open_message.into())
            .await?;
        info!("CertifierService::register_single_signature: created pool '{}' single signature for {signed_entity_type:?}.", single_signature.signer_id);
        debug!("CertifierService::register_single_signature: created single signature for open message ID='{}'.", single_signature.open_message_id);

        Ok(())
    }

    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage> {
        debug!("CertifierService::create_open_message(signed_entity_type: {signed_entity_type:?}, protocol_message: {protocol_message:?})");
        let current_epoch = self.current_epoch.read().await;

        let open_message = self
            .open_message_repository
            .create_open_message(*current_epoch, signed_entity_type, protocol_message)
            .await?;
        info!("CertifierService::create_open_message: created open message for {signed_entity_type:?}");
        debug!(
            "CertifierService::create_open_message: created open message ID='{}'",
            open_message.open_message_id
        );

        Ok(open_message)
    }

    async fn get_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessageWithSingleSignatures>> {
        debug!("CertifierService::get_open_message(signed_entity_type: {signed_entity_type:?})");

        self.open_message_repository
            .get_open_message_with_single_signatures(signed_entity_type)
            .await
    }

    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<Certificate>> {
        debug!("CertifierService::create_certificate(signed_entity_type: {signed_entity_type:?})");
        let open_message = self
            .get_open_message(signed_entity_type)
            .await?
            .ok_or_else(|| {
                warn!("CertifierService::create_certificate: OpenMessage not found for type {signed_entity_type:?}.");
                CertifierServiceError::NotFound(signed_entity_type.clone())
            })?;

        if open_message.is_certified {
            warn!("CertifierService::create_certificate: open message {signed_entity_type:?} is already certified, cannot create certificate.");

            return Err(CertifierServiceError::AlreadyCertified(signed_entity_type.clone()).into());
        }

        let multisigner = self.multi_signer.write().await;
        let signature = multisigner.create_multi_signature(&open_message).await?;

        if signature.is_some() {
            info!("CertifierService::create_certificate: Multi-Signature created for open message {signed_entity_type:?}");
        } else {
            debug!("CertifierService::create_certificate: No multi-signature could be created for open message {signed_entity_type:?}");

            return Ok(None);
        }
        let signature = signature.unwrap();
        let signer_ids = open_message.get_signers_id();
        let signers = multisigner
            .get_signers_with_stake()
            .await?
            .into_iter()
            .filter(|signer| signer_ids.contains(&signer.party_id))
            .collect::<Vec<_>>();

        let protocol_version = PROTOCOL_VERSION.to_string();
        let initiated_at = format!("{:?}", open_message.created_at);
        let sealed_at = format!("{:?}", Utc::now());
        let metadata = CertificateMetadata::new(
            protocol_version,
            // TODO remove this multisigner call ↓
            multisigner.get_protocol_parameters().await?.unwrap().into(),
            initiated_at,
            sealed_at,
            signers,
        );
        let multi_signature = key_encode_hex(signature).map_err(CertificateCreationError::Codec)?;
        let parent_certificate_hash = self
            .certificate_repository
            .get_master_certificate_for_epoch(open_message.epoch)
            .await?
            .map(|cert| cert.hash)
            .ok_or_else(|| CertifierServiceError::NoParentCertificateFound)?;

        let certificate = Certificate::new(
            parent_certificate_hash,
            // TODO: remove this multisigner call ↓
            multisigner.get_current_beacon().await.unwrap(),
            metadata,
            open_message.protocol_message,
            multisigner
                .compute_stake_distribution_aggregate_verification_key()
                .await?
                .unwrap(),
            multi_signature,
            "".to_string(),
        );

        let certificate = self
            .certificate_repository
            .create_certificate(certificate)
            .await?;

        Ok(Some(certificate))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Mutex;

    use mithril_common::entities::Beacon;
    use sqlite::Connection;

    use crate::{dependency_injection::DependenciesBuilder, Configuration};

    use super::*;

    fn setup_dependencies() -> DependenciesBuilder {
        let config = Configuration::new_sample();

        DependenciesBuilder::new(config)
    }
    /*
       fn get_open_message(connection: Arc<Mutex<Connection>>, signed_entity_type: &SignedEntityType) -> StdResult<Option<OpenMessage>> {
           let repository = OpenMessageRepository::new(connection);
           let open_message = repository.get_open_message(epoch, signed_entity_type)
       }
    */
    #[tokio::test]
    async fn create_open_message() {
        let mut dependencies = setup_dependencies();
        let service = dependencies.get_certifier_service().await.unwrap();
        let beacon = Beacon::new("whatever".to_string(), 1, 1);
        let open_message = service
            .create_open_message(
                &SignedEntityType::CardanoImmutableFilesFull(beacon),
                &ProtocolMessage::new(),
            )
            .await
            .unwrap();

        assert!(!open_message.is_certified);

        let connection = dependencies.get_sqlite_connection().await.unwrap();
    }
}
