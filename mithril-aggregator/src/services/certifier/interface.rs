use async_trait::async_trait;
use thiserror::Error;

use mithril_common::entities::{
    Certificate, Epoch, ProtocolMessage, SignedEntityType, SignedEntityTypeDiscriminants,
    SingleSignatures,
};
use mithril_common::{StdError, StdResult};

use crate::entities::OpenMessage;

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

    /// The open message is expired, no more single signatures may be
    /// attached to it nor be certified again.
    #[error("Open message for beacon {0:?} is expired.")]
    Expired(SignedEntityType),

    /// An invalid signature was provided.
    #[error("Invalid single signature for {0:?}.")]
    InvalidSingleSignature(SignedEntityType, #[source] StdError),

    /// No parent certificate could be found, this certifier cannot create genesis certificates.
    #[error(
        "No parent certificate could be found, this certifier cannot create genesis certificates."
    )]
    NoParentCertificateFound,

    /// No certificate for this epoch
    #[error("There is an epoch gap between the last certificate epoch ({certificate_epoch:?}) and current epoch ({current_epoch:?})")]
    CertificateEpochGap {
        /// Epoch of the last issued certificate
        certificate_epoch: Epoch,

        /// Given current epoch
        current_epoch: Epoch,
    },

    /// Could not verify certificate chain because could not find last certificate.
    #[error("No certificate found.")]
    CouldNotFindLastCertificate,
}

/// ## CertifierService
///
/// This service manages the open message and their beacon transitions. It can
/// ultimately transform open messages into certificates.
#[cfg_attr(test, mockall::automock)]
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
    ) -> StdResult<Option<OpenMessage>>;

    /// Mark the open message if it has expired.
    async fn mark_open_message_if_expired(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>>;

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

    /// Returns a certificate from its hash.
    async fn get_certificate_by_hash(&self, hash: &str) -> StdResult<Option<Certificate>>;

    /// Returns the list of the latest created certificates.
    async fn get_latest_certificates(&self, last_n: usize) -> StdResult<Vec<Certificate>>;

    /// Verify the certificate chain and epoch gap. This will return an error if
    /// there is at least an epoch between the given epoch and the most recent
    /// certificate.
    async fn verify_certificate_chain(&self, epoch: Epoch) -> StdResult<()>;
}

/// ## BufferedSignatureStore
///
/// Allow to buffer single signatures for later use when an open message isn't available yet.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait BufferedSingleSignatureStore: Sync + Send {
    /// Buffer a single signature for later use.
    async fn buffer_signature(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        signature: &SingleSignatures,
    ) -> StdResult<()>;

    /// Get the buffered single signatures for the given signed entity discriminant.
    async fn get_buffered_signatures(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<SingleSignatures>>;

    /// Remove the given single signatures from the buffer.
    async fn remove_buffered_signatures(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        single_signatures: Vec<SingleSignatures>,
    ) -> StdResult<()>;
}
