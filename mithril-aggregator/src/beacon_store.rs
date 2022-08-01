use async_trait::async_trait;
use thiserror::Error;

use mithril_common::entities::Beacon;

#[cfg(test)]
use mockall::automock;
use tokio::sync::RwLock;

#[derive(Error, Debug)]
pub enum BeaconStoreError {
    #[error("Generic error")]
    GenericError(),
}

/// BeaconStore represents a beacon store interactor
#[cfg_attr(test, automock)]
#[async_trait]
pub trait BeaconStore: Sync + Send {
    /// Get the current beacon
    async fn get_current_beacon(&self) -> Result<Option<Beacon>, BeaconStoreError>;

    /// Set the current beacon
    async fn set_current_beacon(&self, beacon: Beacon) -> Result<(), BeaconStoreError>;

    /// Reset the current beacon
    async fn reset_current_beacon(&self) -> Result<(), BeaconStoreError>;
}

/// MemoryBeaconStore is in memory [`BeaconStore`]
pub struct MemoryBeaconStore {
    current_beacon: RwLock<Option<Beacon>>,
}

impl MemoryBeaconStore {
    /// MemoryBeaconStore factory
    pub fn new() -> Self {
        Self {
            current_beacon: RwLock::new(None),
        }
    }
}

impl Default for MemoryBeaconStore {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl BeaconStore for MemoryBeaconStore {
    async fn get_current_beacon(&self) -> Result<Option<Beacon>, BeaconStoreError> {
        let beacon = self.current_beacon.read().await;
        Ok(beacon.clone())
    }

    async fn set_current_beacon(&self, beacon: Beacon) -> Result<(), BeaconStoreError> {
        let mut stored_beacon = self.current_beacon.write().await;
        *stored_beacon = Some(beacon);
        Ok(())
    }

    async fn reset_current_beacon(&self) -> Result<(), BeaconStoreError> {
        let mut stored_beacon = self.current_beacon.write().await;
        *stored_beacon = None;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{BeaconStore, MemoryBeaconStore};
    use mithril_common::fake_data;

    #[tokio::test]
    async fn test_can_store_beacon() {
        let sut = MemoryBeaconStore::default();
        let beacon = fake_data::beacon();
        sut.set_current_beacon(beacon.clone())
            .await
            .expect("unexpected error in set_current_beacon");
        let stored_beacon = sut.get_current_beacon().await;

        assert_eq!(Some(beacon), stored_beacon.unwrap());
    }

    #[tokio::test]
    async fn test_reset_current_beacon_ok() {
        let sut = MemoryBeaconStore::default();
        sut.set_current_beacon(fake_data::beacon())
            .await
            .expect("unexpected error in set_current_beacon");
        sut.reset_current_beacon()
            .await
            .expect("unexpected error in set_current_beacon");
        let stored_beacon = sut.get_current_beacon().await;

        assert_eq!(None, stored_beacon.unwrap());
    }
}
