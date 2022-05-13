use crate::beacon_store::BeaconStore;
use std::sync::Arc;
use tokio::sync::RwLock;

use super::entities::*;
use super::multi_signer::MultiSigner;
use super::snapshot_store::SnapshotStorer;

/// BeaconStoreWrapper wraps a BeaconStore
pub type BeaconStoreWrapper = Arc<RwLock<dyn BeaconStore>>;

///  SnapshotStorerWrapper wraps a SnapshotStorer
pub type SnapshotStorerWrapper = Arc<RwLock<dyn SnapshotStorer>>;

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    pub config: Config,
    pub snapshot_storer: Option<SnapshotStorerWrapper>,
    pub multi_signer: Option<MultiSignerWrapper>,
    pub beacon_store: Option<BeaconStoreWrapper>,
}

impl DependencyManager {
    /// DependencyManager factory
    pub fn new(config: Config) -> Self {
        Self {
            config,
            snapshot_storer: None,
            multi_signer: None,
            beacon_store: None,
        }
    }

    /// With SnapshotStorer
    pub fn with_snapshot_storer(&mut self, snapshot_storer: SnapshotStorerWrapper) -> &mut Self {
        self.snapshot_storer = Some(snapshot_storer);
        self
    }

    /// With MultiSigner
    pub fn with_multi_signer(&mut self, multi_signer: MultiSignerWrapper) -> &mut Self {
        self.multi_signer = Some(multi_signer);
        self
    }

    /// With MultiSigner
    pub fn with_beacon_store(&mut self, beacon_store: BeaconStoreWrapper) -> &mut Self {
        self.beacon_store = Some(beacon_store);
        self
    }
}
