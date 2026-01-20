use std::sync::Arc;

use mithril_cardano_node_chain::chain_importer::CardanoChainDataImporter;
use mithril_cardano_node_internal_database::signable_builder::{
    CardanoDatabaseSignableBuilder, CardanoImmutableFilesFullSignableBuilder,
};
use mithril_common::crypto_helper::MKTreeStoreInMemory;
use mithril_common::signable_builder::{
    CardanoStakeDistributionSignableBuilder, CardanoTransactionsSignableBuilder,
    MithrilSignableBuilderService, MithrilStakeDistributionSignableBuilder, SignableBuilderService,
    SignableBuilderServiceDependencies, SignableSeedBuilder,
};

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::get_dependency;
use crate::services::{AggregatorChainDataImporter, AggregatorSignableSeedBuilder};

impl DependenciesBuilder {
    async fn build_signable_builder_service(&mut self) -> Result<Arc<dyn SignableBuilderService>> {
        let seed_signable_builder = self.get_signable_seed_builder().await?;
        let mithril_stake_distribution_builder =
            Arc::new(MithrilStakeDistributionSignableBuilder::default());
        let immutable_signable_builder = Arc::new(CardanoImmutableFilesFullSignableBuilder::new(
            self.get_immutable_digester().await?,
            &self.configuration.db_directory(),
            self.root_logger(),
        ));
        let transaction_importer = self.get_chain_data_importer().await?;
        let block_range_root_retriever = self.get_chain_data_repository().await?;
        let cardano_transactions_builder = Arc::new(CardanoTransactionsSignableBuilder::<
            MKTreeStoreInMemory,
        >::new(
            transaction_importer,
            block_range_root_retriever,
        ));
        let cardano_stake_distribution_builder = Arc::new(
            CardanoStakeDistributionSignableBuilder::new(self.get_stake_store().await?),
        );
        let cardano_database_signable_builder = Arc::new(CardanoDatabaseSignableBuilder::new(
            self.get_immutable_digester().await?,
            &self.configuration.db_directory(),
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
        get_dependency!(self.signable_builder_service)
    }

    async fn build_signable_seed_builder(&mut self) -> Result<Arc<dyn SignableSeedBuilder>> {
        let signable_seed_builder_service = Arc::new(AggregatorSignableSeedBuilder::new(
            self.get_epoch_service().await?,
        ));

        Ok(signable_seed_builder_service)
    }

    /// [SignableSeedBuilder] service
    pub async fn get_signable_seed_builder(&mut self) -> Result<Arc<dyn SignableSeedBuilder>> {
        get_dependency!(self.signable_seed_builder)
    }

    async fn build_chain_data_importer(&mut self) -> Result<Arc<AggregatorChainDataImporter>> {
        let chain_data_importer = Arc::new(CardanoChainDataImporter::new(
            self.get_block_scanner().await?,
            self.get_chain_data_repository().await?,
            self.root_logger(),
        ));

        Ok(Arc::new(AggregatorChainDataImporter::new(
            chain_data_importer,
        )))
    }

    /// Get the [AggregatorChainDataImporter] instance
    pub async fn get_chain_data_importer(&mut self) -> Result<Arc<AggregatorChainDataImporter>> {
        get_dependency!(self.chain_data_importer)
    }
}
