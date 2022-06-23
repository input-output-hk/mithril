use async_trait::async_trait;
use mithril_common::{chain_observer::ChainObserver, digesters::ImmutableFile, entities::Beacon};
use std::{error::Error, path::PathBuf, sync::Arc};
use tokio::sync::RwLock;

use crate::runtime::RuntimeError;

#[async_trait]
pub trait BeaconProvider
where
    Self: Sync + Send,
{
    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn Error + Sync + Send>>;
}
pub struct BeaconProviderImpl {
    observer: Arc<RwLock<dyn ChainObserver>>,
    db_path: PathBuf,
    network: String,
}

impl BeaconProviderImpl {
    pub fn new(observer: Arc<RwLock<dyn ChainObserver>>, db_path: PathBuf, network: &str) -> Self {
        let network = network.to_string();

        Self {
            observer,
            db_path,
            network,
        }
    }
}

#[async_trait]
impl BeaconProvider for BeaconProviderImpl {
    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn Error + Sync + Send>> {
        let epoch = self
            .observer
            .read()
            .await
            .get_current_epoch()
            .await?
            .ok_or_else(|| RuntimeError::General("could not get Epoch".to_string().into()))?;
        let immutable_file_number = ImmutableFile::list_completed_in_dir(&self.db_path)
            .map_err(RuntimeError::ImmutableFile)?
            .into_iter()
            .last()
            .ok_or_else(|| {
                RuntimeError::General("no immutable file was returned".to_string().into())
            })?
            .number;
        let beacon = Beacon {
            network: self.network.clone(),
            epoch,
            immutable_file_number,
        };

        Ok(beacon)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::chain_observer::{ChainObserver, ChainObserverError};
    use mithril_common::entities::{Epoch, StakeDistribution};

    struct TestChainObserver {}

    #[async_trait]
    impl ChainObserver for TestChainObserver {
        async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
            Ok(Some(42))
        }

        async fn get_current_stake_distribution(
            &self,
        ) -> Result<Option<StakeDistribution>, ChainObserverError> {
            let stake_distribution: StakeDistribution = [
                (
                    "pool1qqyjr9pcrv97gwrueunug829fs5znw6p2wxft3fvqkgu5f4qlrg".to_string(),
                    2_493_000 as u64,
                ),
                (
                    "pool1qqfnw2fwajdnam7xsqhhrje5cgd8jcltzfrx655rd23eqlxjfef".to_string(),
                    21_640,
                ),
                (
                    "pool1qqnjh80kudcjphrxftj74x22q3a4uvw8wknlxptgs7gdqtstqad".to_string(),
                    80,
                ),
                (
                    "pool1qquwwu6680fr72y4779r2kpc7mxtch8rp2uhuqcc7v9p6q4f7ph".to_string(),
                    70,
                ),
                (
                    "pool1qptl80vq84xm28pt3t2lhpfzqag28csjhktxz5k6a74n260clmt".to_string(),
                    56,
                ),
                (
                    "pool1qpuckgzxwgdru9vvq3ydmuqa077ur783yn2uywz7zq2c29p506e".to_string(),
                    51_610,
                ),
                (
                    "pool1qz2vzszautc2c8mljnqre2857dpmheq7kgt6vav0s38tvvhxm6w".to_string(),
                    1_051,
                ),
            ]
            .into_iter()
            .collect();

            Ok(Some(stake_distribution))
        }
    }

    #[tokio::test]
    async fn test() {
        let beacon_provider = BeaconProviderImpl::new(
            Arc::new(RwLock::new(TestChainObserver {})),
            PathBuf::new().join("/tmp/db"),
            "whatever",
        );
        let res = beacon_provider.get_current_beacon().await;
        assert!(res.is_err());
    }
}
