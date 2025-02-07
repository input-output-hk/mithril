use std::sync::Arc;

use mithril_common::crypto_helper::MKTreeStoreInMemory;

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::services::{MithrilProverService, ProverService};

impl DependenciesBuilder {
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
}
