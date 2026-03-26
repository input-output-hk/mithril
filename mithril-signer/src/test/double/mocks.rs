use async_trait::async_trait;
use mockall::mock;
use std::collections::BTreeSet;

use mithril_cardano_node_chain::chain_importer::ChainDataImporter;
use mithril_common::{
    StdResult,
    crypto_helper::{MKMap, MKMapNode, MKTreeNode, MKTreeStorer},
    entities::{BlockNumber, BlockRange, CardanoBlockTransactionMkTreeNode, Epoch, TimePoint},
    signable_builder::{BlockRangeRootRetriever, LegacyBlockRangeRootRetriever},
};
use mithril_protocol_config::{
    interface::MithrilNetworkConfigurationProvider, model::MithrilNetworkConfiguration,
};
use mithril_ticker::TickerService;

mock! {
    pub FakeTimePointProvider { }

    #[async_trait]
    impl TickerService for FakeTimePointProvider {
        async fn get_current_time_point(&self) -> StdResult<TimePoint>;
    }
}

mock! {
    pub MithrilNetworkConfigurationProvider {}

    #[async_trait]
    impl MithrilNetworkConfigurationProvider for MithrilNetworkConfigurationProvider {
        async fn get_network_configuration(&self, epoch: Epoch) -> StdResult<MithrilNetworkConfiguration>;
    }
}
mock! {
    pub TickerService {}

    #[async_trait]
    impl TickerService for TickerService {
        async fn get_current_time_point(&self) -> StdResult<TimePoint>;
        async fn get_current_epoch(&self) -> StdResult<Epoch>;
    }
}

mock! {
    pub ChainDataImporter {}

    #[async_trait]
    impl ChainDataImporter for ChainDataImporter {
        async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
    }
}

mock! {
    pub BlockRangeRootRetriever<S: MKTreeStorer> { }

    #[async_trait]
    impl<S: MKTreeStorer> BlockRangeRootRetriever<S> for BlockRangeRootRetriever<S> {
        async fn retrieve_block_range_roots<'a>(
            &'a self,
            up_to_beacon: BlockNumber,
        ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>>;

        async fn retrieve_block_ranges_nodes(
            &self,
            _block_range: BlockRange,
        ) -> StdResult<BTreeSet<CardanoBlockTransactionMkTreeNode>>;

        async fn compute_merkle_map_from_block_range_roots(
            &self,
            up_to_beacon: BlockNumber,
        ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange,S>, S>>;
    }
}

mock! {
    pub LegacyBlockRangeRootRetriever<S: MKTreeStorer> { }

    #[async_trait]
    impl<S: MKTreeStorer> LegacyBlockRangeRootRetriever<S> for LegacyBlockRangeRootRetriever<S> {
        async fn retrieve_block_range_roots<'a>(
            &'a self,
            up_to_beacon: BlockNumber,
        ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>>;

        async fn compute_merkle_map_from_block_range_roots(
            &self,
            up_to_beacon: BlockNumber,
        ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange,S>, S>>;
    }
}
