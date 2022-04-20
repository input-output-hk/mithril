use std::sync::Arc;
use tokio::sync::RwLock;

use crate::entities::*;
use crate::snapshot_store::SnapshotStorer;

pub struct DependencyManager {
    pub config: Config,
    pub snapshot_storer: Option<Arc<RwLock<dyn SnapshotStorer + Sync + Send>>>,
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
    pub fn with_snapshot_storer(
        &mut self,
        snapshot_storer: Arc<RwLock<dyn SnapshotStorer + Sync + Send>>,
    ) -> &mut Self {
        self.snapshot_storer = Some(snapshot_storer);
        self
    }
}
