use async_trait::async_trait;
use mithril_common::chain_observer::{ChainAddress, ChainObserver, ChainObserverError, TxDatum};
use mithril_common::crypto_helper::{KESPeriod, OpCert};
use mithril_common::entities::{ChainPoint, Epoch, StakeDistribution};
use mockall::mock;

mock! {
    pub ChainObserver {}

    #[async_trait]
    impl ChainObserver for ChainObserver {
        async fn get_current_datums(
            &self,
            address: &ChainAddress,
        ) -> Result<Vec<TxDatum>, ChainObserverError>;

        async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError>;

        async fn get_current_chain_point(&self) -> Result<Option<ChainPoint>, ChainObserverError>;

        async fn get_current_stake_distribution(
            &self,
        ) -> Result<Option<StakeDistribution>, ChainObserverError>;

        async fn get_current_kes_period(
            &self,
            opcert: &OpCert,
        ) -> Result<Option<KESPeriod>, ChainObserverError>;
    }
}
