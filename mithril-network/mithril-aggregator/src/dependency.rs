use std::sync::Arc;
use tokio::sync::RwLock;

use crate::entities::*;
use crate::snapshot_store::SnapshotStorer;

///  SnapshotStorerWrapper represents a wrapper of a SnapshotStorer
pub type SnapshotStorerWrapper = Arc<RwLock<dyn SnapshotStorer>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    pub config: Config,
    pub snapshot_storer: Option<SnapshotStorerWrapper>,
}

impl DependencyManager {
    /// DependencyManager factory
    pub fn new(config: Config) -> Self {
        Self {
            config,
            snapshot_storer: None,
        }
    }

    /// With SnapshotStorer
    pub fn with_snapshot_storer(&mut self, snapshot_storer: SnapshotStorerWrapper) -> &mut Self {
        self.snapshot_storer = Some(snapshot_storer);
        self
    }
}
