use anyhow::Context;
use slog::debug;
use std::sync::Arc;
use std::time::Duration;

use mithril_cardano_node_internal_database::digesters::cache::ImmutableFileDigestCacheProvider;
use mithril_persistence::database::repository::CardanoTransactionRepository;

use crate::database::repository::{
    CertificateRepository, EpochSettingsStore, ImmutableFileDigestRepository,
    OpenMessageRepository, SignedEntityStore, SignedEntityStorer, SignerRegistrationStore,
    SignerStore, StakePoolStore,
};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::get_dependency;
use crate::{
    CExplorerSignerRetriever, EpochSettingsStorer, ImmutableFileDigestMapper,
    ProtocolParametersRetriever, SignersImporter, VerificationKeyStorer,
};

impl DependenciesBuilder {
    async fn build_stake_store(&mut self) -> Result<Arc<StakePoolStore>> {
        let stake_pool_store = Arc::new(StakePoolStore::new(
            self.get_sqlite_connection().await?,
            self.configuration.safe_epoch_retention_limit(),
        ));

        Ok(stake_pool_store)
    }

    /// Return a [StakePoolStore]
    pub async fn get_stake_store(&mut self) -> Result<Arc<StakePoolStore>> {
        get_dependency!(self.stake_store)
    }

    async fn build_certificate_repository(&mut self) -> Result<Arc<CertificateRepository>> {
        Ok(Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [CertificateRepository].
    pub async fn get_certificate_repository(&mut self) -> Result<Arc<CertificateRepository>> {
        get_dependency!(self.certificate_repository)
    }

    async fn build_open_message_repository(&mut self) -> Result<Arc<OpenMessageRepository>> {
        Ok(Arc::new(OpenMessageRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [OpenMessageRepository].
    pub async fn get_open_message_repository(&mut self) -> Result<Arc<OpenMessageRepository>> {
        get_dependency!(self.open_message_repository)
    }

    async fn build_verification_key_store(&mut self) -> Result<Arc<dyn VerificationKeyStorer>> {
        Ok(Arc::new(SignerRegistrationStore::new(
            self.get_sqlite_connection().await?,
            self.configuration.safe_epoch_retention_limit(),
        )))
    }

    /// Get a configured [VerificationKeyStorer].
    pub async fn get_verification_key_store(&mut self) -> Result<Arc<dyn VerificationKeyStorer>> {
        get_dependency!(self.verification_key_store)
    }

    async fn build_protocol_parameters_retriever(
        &mut self,
    ) -> Result<Arc<dyn ProtocolParametersRetriever>> {
        let protocol_parameters_retriever =
            EpochSettingsStore::new(self.get_sqlite_connection().await?, None);

        Ok(Arc::new(protocol_parameters_retriever))
    }

    /// Get a configured [ProtocolParametersRetriever].
    pub async fn get_protocol_parameters_retriever(
        &mut self,
    ) -> Result<Arc<dyn ProtocolParametersRetriever>> {
        if self.protocol_parameters_retriever.is_none() {
            self.protocol_parameters_retriever =
                Some(self.build_protocol_parameters_retriever().await?);
        }

        Ok(self.protocol_parameters_retriever.as_ref().cloned().unwrap())
    }

    async fn build_epoch_settings_store(&mut self) -> Result<Arc<EpochSettingsStore>> {
        let logger = self.root_logger();
        let epoch_settings_store = EpochSettingsStore::new(
            self.get_sqlite_connection().await?,
            self.configuration.safe_epoch_retention_limit(),
        );
        let current_epoch = self
            .get_chain_observer()
            .await?
            .get_current_epoch()
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "cannot create aggregator runner: failed to retrieve current epoch."
                    .to_string(),
                error: Some(e.into()),
            })?
            .ok_or(DependenciesBuilderError::Initialization {
                message: "cannot build aggregator runner: no epoch returned.".to_string(),
                error: None,
            })?;
        let retrieval_epoch = current_epoch
            .offset_to_signer_retrieval_epoch()
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: format!("cannot create aggregator runner: failed to offset current epoch '{current_epoch}' to signer retrieval epoch."),
                error: Some(e.into()),
            })?;

        let epoch_settings_configuration = self
            .configuration
            .get_leader_aggregator_epoch_settings_configuration()?;
        debug!(
            logger,
            "Handle discrepancies at startup of epoch settings store, will record epoch settings from the configuration for epoch {retrieval_epoch}";
            "epoch_settings_configuration" => ?epoch_settings_configuration,
        );
        epoch_settings_store
            .handle_discrepancies_at_startup(retrieval_epoch, &epoch_settings_configuration)
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "can not create aggregator runner".to_string(),
                error: Some(e),
            })?;

        Ok(Arc::new(epoch_settings_store))
    }

    /// Get a configured [EpochSettingsStorer].
    pub async fn get_epoch_settings_store(&mut self) -> Result<Arc<EpochSettingsStore>> {
        get_dependency!(self.epoch_settings_store)
    }

    async fn build_immutable_cache_provider(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestCacheProvider>> {
        let cache_provider =
            ImmutableFileDigestRepository::new(self.get_sqlite_connection().await?);
        if self.configuration.reset_digests_cache() {
            cache_provider
                .reset()
                .await
                .with_context(|| "Failure occurred when resetting immutable file digest cache")?;
        }

        Ok(Arc::new(cache_provider))
    }

    /// Get an [ImmutableFileDigestCacheProvider]
    pub async fn get_immutable_cache_provider(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestCacheProvider>> {
        get_dependency!(self.immutable_cache_provider)
    }

    async fn build_transaction_repository(&mut self) -> Result<Arc<CardanoTransactionRepository>> {
        let transaction_store = CardanoTransactionRepository::new(
            self.get_sqlite_connection_cardano_transaction_pool().await?,
        );

        Ok(Arc::new(transaction_store))
    }

    /// Transaction repository.
    pub async fn get_transaction_repository(
        &mut self,
    ) -> Result<Arc<CardanoTransactionRepository>> {
        get_dependency!(self.transaction_repository)
    }

    async fn build_immutable_file_digest_mapper(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestMapper>> {
        let mapper = ImmutableFileDigestRepository::new(self.get_sqlite_connection().await?);

        Ok(Arc::new(mapper))
    }

    /// Immutable digest mapper.
    pub async fn get_immutable_file_digest_mapper(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestMapper>> {
        get_dependency!(self.immutable_file_digest_mapper)
    }

    async fn build_signer_store(&mut self) -> Result<Arc<SignerStore>> {
        let signer_store = Arc::new(SignerStore::new(self.get_sqlite_connection().await?));

        Ok(signer_store)
    }

    /// [SignerStore] service
    pub async fn get_signer_store(&mut self) -> Result<Arc<SignerStore>> {
        get_dependency!(self.signer_store)
    }

    async fn build_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        let signed_entity_storer =
            Arc::new(SignedEntityStore::new(self.get_sqlite_connection().await?));

        Ok(signed_entity_storer)
    }

    /// [SignedEntityStorer] service
    pub async fn get_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        get_dependency!(self.signed_entity_storer)
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
