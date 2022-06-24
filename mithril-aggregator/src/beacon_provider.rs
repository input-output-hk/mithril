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
            let stake_distribution: StakeDistribution = fake_data::signers_with_stakes(5).
            	iter().
            	map(|signer| (signer.party_id.clone() as PartyId, signer.stake as Stake))
            	.collect::<StakeDistribution>();
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
