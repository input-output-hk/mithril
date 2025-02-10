use std::sync::Arc;

use mithril_common::crypto_helper::MKTreeStoreInMemory;
use mithril_common::signable_builder::{
    CardanoDatabaseSignableBuilder, CardanoImmutableFilesFullSignableBuilder,
    CardanoStakeDistributionSignableBuilder, CardanoTransactionsSignableBuilder,
    MithrilSignableBuilderService, MithrilStakeDistributionSignableBuilder, SignableBuilderService,
    SignableBuilderServiceDependencies, SignableSeedBuilder, TransactionsImporter,
};

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::services::{AggregatorSignableSeedBuilder, CardanoTransactionsImporter};

impl DependenciesBuilder {
    async fn build_signable_builder_service(&mut self) -> Result<Arc<dyn SignableBuilderService>> {
        let seed_signable_builder = self.get_signable_seed_builder().await?;
        let mithril_stake_distribution_builder =
            Arc::new(MithrilStakeDistributionSignableBuilder::default());
        let immutable_signable_builder = Arc::new(CardanoImmutableFilesFullSignableBuilder::new(
            self.get_immutable_digester().await?,
            &self.configuration.db_directory,
            self.root_logger(),
        ));
        let transactions_importer = self.get_transactions_importer().await?;
        let block_range_root_retriever = self.get_transaction_repository().await?;
        let cardano_transactions_builder = Arc::new(CardanoTransactionsSignableBuilder::<
            MKTreeStoreInMemory,
        >::new(
            transactions_importer,
            block_range_root_retriever,
        ));
        let cardano_stake_distribution_builder = Arc::new(
            CardanoStakeDistributionSignableBuilder::new(self.get_stake_store().await?),
        );
        let cardano_database_signable_builder = Arc::new(CardanoDatabaseSignableBuilder::new(
            self.get_immutable_digester().await?,
            &self.configuration.db_directory,
            self.root_logger(),
        ));
        let signable_builders_dependencies = SignableBuilderServiceDependencies::new(
            mithril_stake_distribution_builder,
            immutable_signable_builder,
            cardano_transactions_builder,
            cardano_stake_distribution_builder,
            cardano_database_signable_builder,
        );
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
            seed_signable_builder,
            signable_builders_dependencies,
            self.root_logger(),
        ));

        Ok(signable_builder_service)
    }

    /// [SignableBuilderService] service
    pub async fn get_signable_builder_service(
        &mut self,
    ) -> Result<Arc<dyn SignableBuilderService>> {
        if self.signable_builder_service.is_none() {
            self.signable_builder_service = Some(self.build_signable_builder_service().await?);
        }

        Ok(self.signable_builder_service.as_ref().cloned().unwrap())
    }

    async fn build_signable_seed_builder(&mut self) -> Result<Arc<dyn SignableSeedBuilder>> {
        let signable_seed_builder_service = Arc::new(AggregatorSignableSeedBuilder::new(
            self.get_epoch_service().await?,
        ));

        Ok(signable_seed_builder_service)
    }

    /// [SignableSeedBuilder] service
    pub async fn get_signable_seed_builder(&mut self) -> Result<Arc<dyn SignableSeedBuilder>> {
        if self.signable_seed_builder.is_none() {
            self.signable_seed_builder = Some(self.build_signable_seed_builder().await?);
        }

        Ok(self.signable_seed_builder.as_ref().cloned().unwrap())
    }

    async fn build_transactions_importer(&mut self) -> Result<Arc<dyn TransactionsImporter>> {
        let transactions_importer = Arc::new(CardanoTransactionsImporter::new(
            self.get_block_scanner().await?,
            self.get_transaction_repository().await?,
            self.root_logger(),
        ));

        Ok(transactions_importer)
    }

    /// Get the [TransactionsImporter] instance
    pub async fn get_transactions_importer(&mut self) -> Result<Arc<dyn TransactionsImporter>> {
        if self.transactions_importer.is_none() {
            self.transactions_importer = Some(self.build_transactions_importer().await?);
        }

        Ok(self.transactions_importer.as_ref().cloned().unwrap())
    }
}
