use anyhow::Context;
use slog::debug;
use std::sync::Arc;
use std::time::Duration;

use mithril_common::digesters::cache::ImmutableFileDigestCacheProvider;
use mithril_persistence::database::repository::CardanoTransactionRepository;

use crate::database::repository::{
    CertificatePendingRepository, CertificateRepository, EpochSettingsStore,
    ImmutableFileDigestRepository, OpenMessageRepository, SignedEntityStore, SignedEntityStorer,
    SignerRegistrationStore, SignerStore, StakePoolStore,
};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::{
    CExplorerSignerRetriever, CertificatePendingStorer, EpochSettingsStorer,
    ImmutableFileDigestMapper, SignersImporter, VerificationKeyStorer,
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
        if self.stake_store.is_none() {
            self.stake_store = Some(self.build_stake_store().await?);
        }

        Ok(self.stake_store.as_ref().cloned().unwrap())
    }

    async fn build_certificate_pending_storer(
        &mut self,
    ) -> Result<Arc<dyn CertificatePendingStorer>> {
        Ok(Arc::new(CertificatePendingRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [CertificatePendingStorer].
    pub async fn get_certificate_pending_storer(
        &mut self,
    ) -> Result<Arc<dyn CertificatePendingStorer>> {
        if self.certificate_pending_store.is_none() {
            self.certificate_pending_store = Some(self.build_certificate_pending_storer().await?);
        }

        Ok(self.certificate_pending_store.as_ref().cloned().unwrap())
    }

    async fn build_certificate_repository(&mut self) -> Result<Arc<CertificateRepository>> {
        Ok(Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [CertificateRepository].
    pub async fn get_certificate_repository(&mut self) -> Result<Arc<CertificateRepository>> {
        if self.certificate_repository.is_none() {
            self.certificate_repository = Some(self.build_certificate_repository().await?);
        }

        Ok(self.certificate_repository.as_ref().cloned().unwrap())
    }

    async fn build_open_message_repository(&mut self) -> Result<Arc<OpenMessageRepository>> {
        Ok(Arc::new(OpenMessageRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [OpenMessageRepository].
    pub async fn get_open_message_repository(&mut self) -> Result<Arc<OpenMessageRepository>> {
        if self.open_message_repository.is_none() {
            self.open_message_repository = Some(self.build_open_message_repository().await?);
        }

        Ok(self.open_message_repository.as_ref().cloned().unwrap())
    }

    async fn build_verification_key_store(&mut self) -> Result<Arc<dyn VerificationKeyStorer>> {
        Ok(Arc::new(SignerRegistrationStore::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [VerificationKeyStorer].
    pub async fn get_verification_key_store(&mut self) -> Result<Arc<dyn VerificationKeyStorer>> {
        if self.verification_key_store.is_none() {
            self.verification_key_store = Some(self.build_verification_key_store().await?);
        }

        Ok(self.verification_key_store.as_ref().cloned().unwrap())
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

        {
            // Temporary fix, should be removed
            // Replace empty JSON values '{}' injected with Migration #28
            let cardano_signing_config = self
                .configuration
                .cardano_transactions_signing_config
                .clone();
            #[allow(deprecated)]
            epoch_settings_store
                .replace_cardano_signing_config_empty_values(cardano_signing_config)?;
        }

        let epoch_settings_configuration = self.get_epoch_settings_configuration()?;
        debug!(
            logger,
            "Handle discrepancies at startup of epoch settings store, will record epoch settings from the configuration for epoch {current_epoch}";
            "epoch_settings_configuration" => ?epoch_settings_configuration,
        );
        epoch_settings_store
            .handle_discrepancies_at_startup(current_epoch, &epoch_settings_configuration)
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "can not create aggregator runner".to_string(),
                error: Some(e),
            })?;

        Ok(Arc::new(epoch_settings_store))
    }

    /// Get a configured [EpochSettingsStorer].
    pub async fn get_epoch_settings_store(&mut self) -> Result<Arc<EpochSettingsStore>> {
        if self.epoch_settings_store.is_none() {
            self.epoch_settings_store = Some(self.build_epoch_settings_store().await?);
        }

        Ok(self.epoch_settings_store.as_ref().cloned().unwrap())
    }

    async fn build_immutable_cache_provider(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestCacheProvider>> {
        let cache_provider =
            ImmutableFileDigestRepository::new(self.get_sqlite_connection().await?);
        if self.configuration.reset_digests_cache {
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
        if self.immutable_cache_provider.is_none() {
            self.immutable_cache_provider = Some(self.build_immutable_cache_provider().await?);
        }

        Ok(self.immutable_cache_provider.as_ref().cloned().unwrap())
    }

    async fn build_transaction_repository(&mut self) -> Result<Arc<CardanoTransactionRepository>> {
        let transaction_store = CardanoTransactionRepository::new(
            self.get_sqlite_connection_cardano_transaction_pool()
                .await?,
        );

        Ok(Arc::new(transaction_store))
    }

    /// Transaction repository.
    pub async fn get_transaction_repository(
        &mut self,
    ) -> Result<Arc<CardanoTransactionRepository>> {
        if self.transaction_repository.is_none() {
            self.transaction_repository = Some(self.build_transaction_repository().await?);
        }

        Ok(self.transaction_repository.as_ref().cloned().unwrap())
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
        if self.immutable_file_digest_mapper.is_none() {
            self.immutable_file_digest_mapper =
                Some(self.build_immutable_file_digest_mapper().await?);
        }

        Ok(self.immutable_file_digest_mapper.as_ref().cloned().unwrap())
    }

    async fn build_signer_store(&mut self) -> Result<Arc<SignerStore>> {
        let signer_store = Arc::new(SignerStore::new(self.get_sqlite_connection().await?));

        Ok(signer_store)
    }

    /// [SignerStore] service
    pub async fn get_signer_store(&mut self) -> Result<Arc<SignerStore>> {
        match self.signer_store.as_ref().cloned() {
            None => {
                let store = self.build_signer_store().await?;
                self.signer_store = Some(store.clone());
                Ok(store)
            }
            Some(store) => Ok(store),
        }
    }

    async fn build_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        let signed_entity_storer =
            Arc::new(SignedEntityStore::new(self.get_sqlite_connection().await?));

        Ok(signed_entity_storer)
    }

    /// [SignedEntityStorer] service
    pub async fn get_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        if self.signed_entity_storer.is_none() {
            self.signed_entity_storer = Some(self.build_signed_entity_storer().await?);
        }

        Ok(self.signed_entity_storer.as_ref().cloned().unwrap())
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
