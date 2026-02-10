use async_trait::async_trait;
use rayon::prelude::*;
use slog::{Logger, debug, info};
use std::sync::Arc;

use mithril_common::{
    StdResult,
    crypto_helper::{MKMap, MKMapNode, MKTreeStorer},
    entities::{BlockNumber, BlockRange},
    logging::LoggerExtensions,
    signable_builder::LegacyBlockRangeRootRetriever,
};
use mithril_resource_pool::ResourcePool;

use crate::services::TransactionsRetriever;

/// Prover service is the cryptographic engine in charge of producing cryptographic proofs for transactions and blocks
// TODO: finalize the trait implementation
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ProverService: Sync + Send {
    /// Compute the cache
    async fn compute_cache(&self, up_to: BlockNumber) -> StdResult<()>;
}

/// Mithril prover
pub struct MithrilProverService<S: MKTreeStorer> {
    _transaction_retriever: Arc<dyn TransactionsRetriever>,
    block_range_root_retriever: Arc<dyn LegacyBlockRangeRootRetriever<S>>,
    mk_map_pool: ResourcePool<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>,
    logger: Logger,
}

impl<S: MKTreeStorer> MithrilProverService<S> {
    /// Create a new Mithril prover
    pub fn new(
        _transaction_retriever: Arc<dyn TransactionsRetriever>,
        block_range_root_retriever: Arc<dyn LegacyBlockRangeRootRetriever<S>>,
        mk_map_pool_size: usize,
        logger: Logger,
    ) -> Self {
        Self {
            _transaction_retriever,
            block_range_root_retriever,
            mk_map_pool: ResourcePool::new(mk_map_pool_size, vec![]),
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl<S: MKTreeStorer> ProverService for MithrilProverService<S> {
    async fn compute_cache(&self, up_to: BlockNumber) -> StdResult<()> {
        let pool_size = self.mk_map_pool.size();
        info!(
            self.logger, "Starts computing the Merkle map pool resource of size {pool_size}";
            "up_to_block_number" => *up_to,
        );
        let mk_map_cache = self
            .block_range_root_retriever
            .compute_merkle_map_from_block_range_roots(up_to)
            .await?;
        let mk_maps_new = (1..=pool_size)
            .into_par_iter()
            .map(|i| {
                debug!(
                    self.logger,
                    "Computing the Merkle map pool resource {i}/{pool_size}"
                );
                mk_map_cache.clone()
            })
            .collect::<Vec<MKMap<_, _, _>>>();
        debug!(self.logger, "Draining the Merkle map pool");
        let discriminant_new = self.mk_map_pool.discriminant()? + 1;
        self.mk_map_pool.set_discriminant(discriminant_new)?;
        self.mk_map_pool.clear();
        debug!(
            self.logger,
            "Giving back new resources to the Merkle map pool"
        );
        mk_maps_new
            .into_iter()
            .map(|mk_map| self.mk_map_pool.give_back_resource(mk_map, discriminant_new))
            .collect::<StdResult<Vec<_>>>()?;
        info!(
            self.logger,
            "Completed computing the Merkle map pool resource of size {pool_size}"
        );

        Ok(())
    }
}
