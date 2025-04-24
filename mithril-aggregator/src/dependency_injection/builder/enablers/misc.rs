//! Miscellaneous enablers
//!
//! This naming is not ideal, we should either:
//! - group these enablers into more logical categories
//! - redefine the actual categories so those miscellaneous enablers fit into them

use std::sync::Arc;
use std::time::Duration;

use mithril_signed_entity_lock::SignedEntityTypeLock;

use crate::database::repository::CertificateRepository;
use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::get_dependency;
use crate::services::{
    AggregatorClient, AggregatorHTTPClient, MessageService, MithrilMessageService,
};
impl DependenciesBuilder {
    async fn build_signed_entity_type_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        let signed_entity_lock = Arc::new(SignedEntityTypeLock::default());
        Ok(signed_entity_lock)
    }

    /// Get the [SignedEntityTypeLock] instance
    pub async fn get_signed_entity_type_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        get_dependency!(self.signed_entity_type_lock)
    }

    /// build HTTP message service
    pub async fn build_message_service(&mut self) -> Result<Arc<dyn MessageService>> {
        let certificate_repository = Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let signed_entity_storer = self.get_signed_entity_storer().await?;
        let immutable_file_digest_mapper = self.get_immutable_file_digest_mapper().await?;
        let epoch_service = self.get_epoch_service().await?;
        let service = MithrilMessageService::new(
            certificate_repository,
            signed_entity_storer,
            immutable_file_digest_mapper,
            epoch_service,
        );

        Ok(Arc::new(service))
    }

    /// [MessageService] service
    pub async fn get_message_service(&mut self) -> Result<Arc<dyn MessageService>> {
        get_dependency!(self.message_service)
    }

    /// build an [AggregatorClient]
    pub async fn build_leader_aggregator_client(&mut self) -> Result<Arc<dyn AggregatorClient>> {
        let leader_aggregator_endpoint = self
            .configuration
            .leader_aggregator_endpoint()
            .unwrap_or_default();
        let aggregator_client = AggregatorHTTPClient::new(
            leader_aggregator_endpoint,
            None,
            self.get_api_version_provider().await?,
            Some(Duration::from_secs(30)),
            self.root_logger(),
        );

        Ok(Arc::new(aggregator_client))
    }

    /// Returns a leader [AggregatorClient]
    pub async fn get_leader_aggregator_client(&mut self) -> Result<Arc<dyn AggregatorClient>> {
        get_dependency!(self.leader_aggregator_client)
    }
}
