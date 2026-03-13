use async_trait::async_trait;
use mockall::mock;

use mithril_cardano_node_chain::{
    chain_observer::{ChainObserver, ChainObserverError},
    entities::{ChainAddress, TxDatum},
};
use mithril_common::{
    StdResult,
    certificate_chain::CertificateVerifier,
    crypto_helper::{
        KesPeriod, MKMap, MKMapNode, MKTreeNode, MKTreeStorer, ProtocolGenesisVerificationKey,
    },
    entities::{BlockNumber, BlockRange, Certificate, ChainPoint, Epoch, StakeDistribution},
    signable_builder::BlockRangeRootRetriever,
};
use mithril_persistence::store::StakeStorer;

mock! {
    pub CertificateVerifier {}

    #[async_trait]
    impl CertificateVerifier for CertificateVerifier {
        async fn verify_genesis_certificate(
            &self,
            genesis_certificate: &Certificate,
            genesis_verification_key: &ProtocolGenesisVerificationKey,
        ) -> StdResult<()>;

        async fn verify_standard_certificate(
            &self,
            certificate: &Certificate,
            previous_certificate: &Certificate,
        ) -> StdResult<()>;

        async fn verify_certificate(
            &self,
            certificate: &Certificate,
            genesis_verification_key: &ProtocolGenesisVerificationKey,
        ) -> StdResult<Option<Certificate>>;

        async fn verify_certificate_chain(
            &self,
            certificate: Certificate,
            genesis_verification_key: &ProtocolGenesisVerificationKey,
        ) -> StdResult<()>;
    }
}

mock! {
    pub ChainObserver {}

    #[async_trait]
    impl ChainObserver for ChainObserver {
        async fn get_current_datums(
            &self,
            address: &ChainAddress,
        ) -> Result<Vec<TxDatum>, ChainObserverError>;

        async fn get_current_era(&self) -> Result<Option<String>, ChainObserverError>;

        async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

        async fn get_current_chain_point(&self) -> Result<Option<ChainPoint>, ChainObserverError>;

        async fn get_current_stake_distribution(
            &self,
        ) -> Result<Option<StakeDistribution>, ChainObserverError>;

        async fn get_current_kes_period(
            &self,
        ) -> Result<Option<KesPeriod>, ChainObserverError>;
    }
}

mock! {
    pub StakeStore {}

    #[async_trait]
    impl StakeStorer for StakeStore {
        async fn save_stakes(&self, epoch: Epoch, stakes: StakeDistribution) -> StdResult<Option<StakeDistribution>>;

        async fn get_stakes(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>>;
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

        async fn compute_merkle_map_from_block_range_roots(
            &self,
            up_to_beacon: BlockNumber,
        ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>;
    }
}
