//! This service is responsible of providing HTTP server with messages as fast as possible.

use std::sync::Arc;

use async_trait::async_trait;
use thiserror::Error;

use mithril_common::{
    messages::{CertificateListMessage, CertificateMessage, SnapshotMessage},
    StdResult,
};

use crate::database::provider::{CertificateRepository, SignedEntityStorer};

#[cfg(test)]
use mockall::automock;

/// Error related to the [HttpMessageService]
#[derive(Debug, Error)]
pub enum HttpMessageServiceError {
    /// There is no current PendingCertificate
    #[error("There is no current pending certificate.")]
    PendingCertificateDoesNotExist,
}
/// HTTP Message service trait.
#[cfg_attr(test, automock)]
#[async_trait]
pub trait HttpMessageService: Sync + Send {
    /// Return the message representation of a certificate if it exists.
    async fn get_certificate(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>>;

    /// Return the message representation of the last N certificates
    async fn get_last_certificates(&self, limit: usize) -> StdResult<CertificateListMessage>;

    /// Return the information regarding the given snapshot
    async fn get_snapshot_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SnapshotMessage>>;
}

/// Implementation of the [HttpMessageService]
pub struct MithrilHttpMessageService {
    certificate_repository: Arc<CertificateRepository>,
    signed_entity_storer: Arc<dyn SignedEntityStorer>,
}

impl MithrilHttpMessageService {
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
impl HttpMessageService for MithrilHttpMessageService {
    async fn get_certificate(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<CertificateMessage>> {
        self.certificate_repository
            .get_certificate(certificate_hash)
            .await
    }

    async fn get_last_certificates(&self, limit: usize) -> StdResult<CertificateListMessage> {
        self.certificate_repository
            .get_latest_certificates(limit)
            .await
    }

    async fn get_snapshot_message(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SnapshotMessage>> {
        let signed_entity = match self
            .signed_entity_storer
            .get_signed_entity(signed_entity_id)
            .await?
        {
            Some(v) => v,
            None => return Ok(None),
        };

        Ok(Some(signed_entity.try_into()?))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Beacon, test_utils::MithrilFixtureBuilder};

    use crate::{dependency_injection::DependenciesBuilder, Configuration};

    #[tokio::test]
    async fn get_no_certificate() {
        // setup
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let service = dep_builder.get_http_message_service().await.unwrap();

        // test
        let certificate_hash = "whatever";
        let certficate_message = service.get_certificate(certificate_hash).await.unwrap();
        assert!(certficate_message.is_none());
    }

    #[tokio::test]
    async fn get_certificate() {
        // setup
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let service = dep_builder.get_http_message_service().await.unwrap();
        let beacon = Beacon::new("devnet".to_string(), 3, 1);
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let genesis_beacon = Beacon {
            epoch: beacon.epoch - 1,
            ..beacon.clone()
        };
        let genesis_certificate = fixture.create_genesis_certificate(&genesis_beacon);
        dep_builder
            .get_certificate_repository()
            .await
            .unwrap()
            .create_certificate(genesis_certificate.clone())
            .await
            .unwrap();

        // test
        let certficate_message = service
            .get_certificate(&genesis_certificate.hash)
            .await
            .unwrap()
            .expect("There should be a certificate.");
        assert_eq!(genesis_certificate.hash, certficate_message.hash);
    }

    #[tokio::test]
    async fn get_last_certificates() {
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let service = dep_builder.get_http_message_service().await.unwrap();
        let beacon = Beacon::new("devnet".to_string(), 3, 1);
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let genesis_beacon = Beacon {
            epoch: beacon.epoch - 1,
            ..beacon.clone()
        };
        let genesis_certificate = fixture.create_genesis_certificate(&genesis_beacon);
        dep_builder
            .get_certificate_repository()
            .await
            .unwrap()
            .create_certificate(genesis_certificate.clone())
            .await
            .unwrap();
        let genesis_certificate = fixture.create_genesis_certificate(&beacon);
        dep_builder
            .get_certificate_repository()
            .await
            .unwrap()
            .create_certificate(genesis_certificate.clone())
            .await
            .unwrap();

        // test
        let certficate_messages = service.get_last_certificates(5).await.unwrap();

        assert_eq!(2, certficate_messages.len());
        assert_eq!(genesis_certificate.hash, certficate_messages[0].hash);
    }

    #[tokio::test]
    async fn get_no_snapshot() {
        let configuration = Configuration::new_sample();
        let mut dep_builder = DependenciesBuilder::new(configuration);
        let service = dep_builder.get_http_message_service().await.unwrap();
        let snapshot = service.get_snapshot_message("whatever").await.unwrap();

        assert!(snapshot.is_none());
    }
}
