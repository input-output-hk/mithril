//! Builder methods for services
//!
//! Services encapsulate the business logic of the application.
//!

mod artifacts;
mod certification;
mod era;
mod signables;

use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

use mithril_common::api_version::APIVersionProvider;
use mithril_common::crypto_helper::MKTreeStoreInMemory;
use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;
use mithril_common::{MithrilTickerService, TickerService};

use crate::database::repository::CertificateRepository;
use crate::dependency_injection::{DependenciesBuilder, EpochServiceWrapper, Result};
use crate::event_store::{EventMessage, TransmitterService};
use crate::services::{
    AggregatorUpkeepService, EpochServiceDependencies, MessageService, MithrilEpochService,
    MithrilMessageService, MithrilProverService, MithrilStakeDistributionService, ProverService,
    StakeDistributionService, UpkeepService, UsageReporter,
};
use crate::{
    CExplorerSignerRetriever, MetricsService, MithrilSignerRegisterer, SignersImporter,
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

    async fn build_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        let verification_key_store = self.get_verification_key_store().await?;
        let epoch_settings_storer = self.get_epoch_settings_store().await?;
        let chain_observer = self.get_chain_observer().await?;
        let era_checker = self.get_era_checker().await?;
        let stake_distribution_service = self.get_stake_distribution_service().await?;
        let epoch_settings = self.configuration.get_epoch_settings_configuration();
        let allowed_discriminants = self
            .configuration
            .compute_allowed_signed_entity_types_discriminants()?;

        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(
            epoch_settings,
            EpochServiceDependencies::new(
                epoch_settings_storer,
                verification_key_store,
                chain_observer,
                era_checker,
                stake_distribution_service,
            ),
            allowed_discriminants,
            self.root_logger(),
        )));

        Ok(epoch_service)
    }

    /// [EpochService][crate::services::EpochService] service
    pub async fn get_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        if self.epoch_service.is_none() {
            self.epoch_service = Some(self.build_epoch_service().await?);
        }

        Ok(self.epoch_service.as_ref().cloned().unwrap())
    }

    async fn build_event_transmitter(&mut self) -> Result<Arc<TransmitterService<EventMessage>>> {
        let sender = self.get_event_transmitter_sender().await?;
        let event_transmitter = Arc::new(TransmitterService::new(sender, self.root_logger()));

        Ok(event_transmitter)
    }

    /// [TransmitterService] service
    pub async fn get_event_transmitter(&mut self) -> Result<Arc<TransmitterService<EventMessage>>> {
        if self.event_transmitter.is_none() {
            self.event_transmitter = Some(self.build_event_transmitter().await?);
        }

        Ok(self.event_transmitter.as_ref().cloned().unwrap())
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

    /// Create a [MetricsService] instance.
    async fn build_metrics_service(&self) -> Result<Arc<MetricsService>> {
        let metrics_service = MetricsService::new(self.root_logger())?;

        Ok(Arc::new(metrics_service))
    }

    /// [MetricsService] service
    pub async fn get_metrics_service(&mut self) -> Result<Arc<MetricsService>> {
        if self.metrics_service.is_none() {
            self.metrics_service = Some(self.build_metrics_service().await?);
        }

        Ok(self.metrics_service.as_ref().cloned().unwrap())
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

    async fn build_stake_distribution_service(
        &mut self,
    ) -> Result<Arc<dyn StakeDistributionService>> {
        let stake_distribution_service = Arc::new(MithrilStakeDistributionService::new(
            self.get_stake_store().await?,
            self.get_chain_observer().await?,
        ));

        Ok(stake_distribution_service)
    }

    /// [StakeDistributionService] service
    pub async fn get_stake_distribution_service(
        &mut self,
    ) -> Result<Arc<dyn StakeDistributionService>> {
        if self.stake_distribution_service.is_none() {
            self.stake_distribution_service = Some(self.build_stake_distribution_service().await?);
        }

        Ok(self.stake_distribution_service.as_ref().cloned().unwrap())
    }

    /// Create [TickerService] instance.
    pub async fn build_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        let chain_observer = self.get_chain_observer().await?;
        let immutable_observer = self.get_immutable_file_observer().await?;

        Ok(Arc::new(MithrilTickerService::new(
            chain_observer,
            immutable_observer,
        )))
    }

    /// [StakeDistributionService] service
    pub async fn get_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        if self.ticker_service.is_none() {
            self.ticker_service = Some(self.build_ticker_service().await?);
        }

        Ok(self.ticker_service.as_ref().cloned().unwrap())
    }

    /// Create a [UsageReporter] instance.
    pub async fn create_usage_reporter(&mut self) -> Result<UsageReporter> {
        let usage_reporter = UsageReporter::new(
            self.get_event_transmitter().await?,
            self.get_metrics_service().await?,
            self.root_logger(),
        );

        Ok(usage_reporter)
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
