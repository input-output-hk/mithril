use async_trait::async_trait;

use mithril_common::chain_observer::{ChainAddress, ChainObserver, ChainObserverError, TxDatum};
use mithril_common::crypto_helper::{KESPeriod, OpCert};
use mithril_common::entities::{ChainPoint, Epoch, StakeDistribution};
use mithril_persistence::store::StakeStorer;

use mithril_common::StdResult;
use mockall::mock;

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
            opcert: &OpCert,
        ) -> Result<Option<KESPeriod>, ChainObserverError>;
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
