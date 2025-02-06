//! Builder methods for services
//!
//! Services encapsulate the business logic of the application.
//!

mod artifacts;
mod certification;
mod era;
mod pacing;
mod reporting;
mod signables;

use std::sync::Arc;
use std::time::Duration;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::crypto_helper::MKTreeStoreInMemory;
use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;

use crate::database::repository::CertificateRepository;
use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::services::{
    AggregatorUpkeepService, MessageService, MithrilMessageService, MithrilProverService,
    ProverService, UpkeepService,
};
use crate::{
    CExplorerSignerRetriever, MithrilSignerRegisterer, SignersImporter,
    SingleSignatureAuthenticator,
};

impl DependenciesBuilder {
    async fn build_api_version_provider(&mut self) -> Result<Arc<APIVersionProvider>> {
        let api_version_provider = Arc::new(APIVersionProvider::new(self.get_era_checker().await?));

        Ok(api_version_provider)
    }

    /// Get the [APIVersionProvider] instance
    pub async fn get_api_version_provider(&mut self) -> Result<Arc<APIVersionProvider>> {
        if self.api_version_provider.is_none() {
            self.api_version_provider = Some(self.build_api_version_provider().await?);
        }

        Ok(self.api_version_provider.as_ref().cloned().unwrap())
    }

    /// build HTTP message service
    pub async fn build_message_service(&mut self) -> Result<Arc<dyn MessageService>> {
        let certificate_repository = Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let signed_entity_storer = self.get_signed_entity_storer().await?;
        let immutable_file_digest_mapper = self.get_immutable_file_digest_mapper().await?;
        let service = MithrilMessageService::new(
            certificate_repository,
            signed_entity_storer,
            immutable_file_digest_mapper,
        );

        Ok(Arc::new(service))
    }

    /// [MessageService] service
    pub async fn get_message_service(&mut self) -> Result<Arc<dyn MessageService>> {
        if self.message_service.is_none() {
            self.message_service = Some(self.build_message_service().await?);
        }

        Ok(self.message_service.as_ref().cloned().unwrap())
    }

    /// Build Prover service
    pub async fn build_prover_service(&mut self) -> Result<Arc<dyn ProverService>> {
        let mk_map_pool_size = self
            .configuration
            .cardano_transactions_prover_cache_pool_size;
        let transaction_retriever = self.get_transaction_repository().await?;
        let block_range_root_retriever = self.get_transaction_repository().await?;
        let logger = self.root_logger();
        let prover_service = MithrilProverService::<MKTreeStoreInMemory>::new(
            transaction_retriever,
            block_range_root_retriever,
            mk_map_pool_size,
            logger,
        );

        Ok(Arc::new(prover_service))
    }

    /// [ProverService] service
    pub async fn get_prover_service(&mut self) -> Result<Arc<dyn ProverService>> {
        if self.prover_service.is_none() {
            self.prover_service = Some(self.build_prover_service().await?);
        }

        Ok(self.prover_service.as_ref().cloned().unwrap())
    }

    async fn build_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        let stake_pool_pruning_task = self.get_stake_store().await?;
        let epoch_settings_pruning_task = self.get_epoch_settings_store().await?;
        let mithril_registerer_pruning_task = self.get_mithril_registerer().await?;

        let upkeep_service = Arc::new(AggregatorUpkeepService::new(
            self.get_sqlite_connection().await?,
            self.get_sqlite_connection_cardano_transaction_pool()
                .await?,
            self.get_event_store_sqlite_connection().await?,
            self.get_signed_entity_lock().await?,
            vec![
                stake_pool_pruning_task,
                epoch_settings_pruning_task,
                mithril_registerer_pruning_task,
            ],
            self.root_logger(),
        ));

        Ok(upkeep_service)
    }

    /// [UpkeepService] service
    pub async fn get_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        if self.upkeep_service.is_none() {
            self.upkeep_service = Some(self.build_upkeep_service().await?);
        }

        Ok(self.upkeep_service.as_ref().cloned().unwrap())
    }

    async fn build_mithril_registerer(&mut self) -> Result<Arc<MithrilSignerRegisterer>> {
        let registerer = MithrilSignerRegisterer::new(
            self.get_chain_observer().await?,
            self.get_verification_key_store().await?,
            self.get_signer_store().await?,
            self.configuration.safe_epoch_retention_limit(),
        );

        Ok(Arc::new(registerer))
    }

    /// Get the [MithrilSignerRegisterer] instance
    pub async fn get_mithril_registerer(&mut self) -> Result<Arc<MithrilSignerRegisterer>> {
        if self.mithril_registerer.is_none() {
            self.mithril_registerer = Some(self.build_mithril_registerer().await?);
        }

        Ok(self.mithril_registerer.as_ref().cloned().unwrap())
    }

    async fn build_signed_entity_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
        Ok(signed_entity_type_lock)
    }

    /// Get the [SignedEntityTypeLock] instance
    pub async fn get_signed_entity_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        if self.signed_entity_type_lock.is_none() {
            self.signed_entity_type_lock = Some(self.build_signed_entity_lock().await?);
        }

        Ok(self.signed_entity_type_lock.as_ref().cloned().unwrap())
    }

    async fn build_single_signature_authenticator(
        &mut self,
    ) -> Result<Arc<SingleSignatureAuthenticator>> {
        let authenticator =
            SingleSignatureAuthenticator::new(self.get_multi_signer().await?, self.root_logger());

        Ok(Arc::new(authenticator))
    }

    /// Get the [SingleSignatureAuthenticator] instance
    pub async fn get_single_signature_authenticator(
        &mut self,
    ) -> Result<Arc<SingleSignatureAuthenticator>> {
        if self.single_signer_authenticator.is_none() {
            self.single_signer_authenticator =
                Some(self.build_single_signature_authenticator().await?);
        }

        Ok(self.single_signer_authenticator.as_ref().cloned().unwrap())
    }

    /// Create a [SignersImporter] instance.
    pub async fn create_signer_importer(
        &mut self,
        cexplorer_pools_url: &str,
    ) -> Result<SignersImporter> {
        let retriever = CExplorerSignerRetriever::new(
            cexplorer_pools_url,
            Some(Duration::from_secs(30)),
            self.root_logger(),
        )?;
        let persister = self.get_signer_store().await?;

        Ok(SignersImporter::new(
            Arc::new(retriever),
            persister,
            self.root_logger(),
        ))
    }
}
