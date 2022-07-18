use async_trait::async_trait;
use std::{error::Error, path::PathBuf, sync::Arc};
use tokio::sync::RwLock;

use mithril_common::{
    chain_observer::ChainObserver, digesters::ImmutableFile, entities::Beacon, CardanoNetwork,
};

use crate::runtime::RuntimeError;

#[async_trait]
pub trait ImmutableFileObserver
where
    Self: Sync + Send,
{
    async fn get_last_immutable_number(&self) -> Result<u64, Box<dyn Error + Sync + Send>>;
}

pub struct ImmutableFileSystemObserver {
    db_path: PathBuf,
}

impl ImmutableFileSystemObserver {
    pub fn new(db_path: &PathBuf) -> Self {
        let db_path = db_path.to_owned();

        Self { db_path }
    }
}

#[async_trait]
impl ImmutableFileObserver for ImmutableFileSystemObserver {
    async fn get_last_immutable_number(&self) -> Result<u64, Box<dyn Error + Sync + Send>> {
        let immutable_file_number = ImmutableFile::list_completed_in_dir(&self.db_path)
            .map_err(RuntimeError::ImmutableFile)?
            .into_iter()
            .last()
            .ok_or_else(|| {
                RuntimeError::General("no immutable file was returned".to_string().into())
            })?
            .number;

        Ok(immutable_file_number)
    }
}

#[async_trait]
pub trait BeaconProvider
where
    Self: Sync + Send,
{
    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn Error + Sync + Send>>;
}

pub struct BeaconProviderImpl {
    chain_observer: Arc<RwLock<dyn ChainObserver>>,
    immutable_observer: Arc<RwLock<dyn ImmutableFileObserver>>,
    network: CardanoNetwork,
}

impl BeaconProviderImpl {
    pub fn new(
        chain_observer: Arc<RwLock<dyn ChainObserver>>,
        immutable_observer: Arc<RwLock<dyn ImmutableFileObserver>>,
        network: CardanoNetwork,
    ) -> Self {
        Self {
            chain_observer,
            immutable_observer,
            network,
        }
    }
}

#[async_trait]
impl BeaconProvider for BeaconProviderImpl {
    async fn get_current_beacon(&self) -> Result<Beacon, Box<dyn Error + Sync + Send>> {
        let epoch = self
            .chain_observer
            .read()
            .await
            .get_current_epoch()
            .await?
            .ok_or_else(|| RuntimeError::General("could not get Epoch".to_string().into()))?;
        let immutable_file_number = self
            .immutable_observer
            .read()
            .await
            .get_last_immutable_number()
            .await?;

        let beacon = Beacon {
            network: self.network.to_string(),
            epoch,
            immutable_file_number,
        };

        Ok(beacon)
    }
}

pub struct DumbImmutableFileObserver {
    shall_return: Option<u64>,
}

impl Default for DumbImmutableFileObserver {
    fn default() -> Self {
        let mut observer = Self::new();
        observer.shall_return(Some(119827));

        observer
    }
}

impl DumbImmutableFileObserver {
    pub fn new() -> Self {
        Self { shall_return: None }
    }

    pub fn shall_return(&mut self, what: Option<u64>) -> &mut Self {
        self.shall_return = what;
        self
    }
}

#[async_trait]
impl ImmutableFileObserver for DumbImmutableFileObserver {
    async fn get_last_immutable_number(&self) -> Result<u64, Box<dyn Error + Sync + Send>> {
        self.shall_return
            .ok_or_else(|| "fake immutable error".into())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::chain_observer::{ChainObserver, ChainObserverError};
    use mithril_common::entities::{Epoch, StakeDistribution};

    use super::*;

    struct DumbChainObserver {}

    #[async_trait]
    impl ChainObserver for DumbChainObserver {
        async fn get_current_epoch(&self) -> Result<Option<Epoch>, ChainObserverError> {
            Ok(Some(42))
        }

        async fn get_current_stake_distribution(
            &self,
        ) -> Result<Option<StakeDistribution>, ChainObserverError> {
            Err(ChainObserverError::General(
                "this should not be called in the BeaconProvider"
                    .to_string()
                    .into(),
            ))
        }
    }

    #[tokio::test]
    async fn test_beacon_ok() {
        let beacon_provider = BeaconProviderImpl::new(
            Arc::new(RwLock::new(DumbChainObserver {})),
            Arc::new(RwLock::new(DumbImmutableFileObserver::default())),
            CardanoNetwork::TestNet(42),
        );
        let beacon = beacon_provider.get_current_beacon().await.unwrap();

        assert_eq!(42, beacon.epoch);
        assert_eq!(119_827, beacon.immutable_file_number);
    }

    #[tokio::test]
    async fn test_beacon_error() {
        let mut immutable_observer = DumbImmutableFileObserver::default();
        immutable_observer.shall_return(None);
        let beacon_provider = BeaconProviderImpl::new(
            Arc::new(RwLock::new(DumbChainObserver {})),
            Arc::new(RwLock::new(immutable_observer)),
            CardanoNetwork::TestNet(42),
        );

        let result = beacon_provider.get_current_beacon().await;
        assert!(result.is_err());
    }
}
