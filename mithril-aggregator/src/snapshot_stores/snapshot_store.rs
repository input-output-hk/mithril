use async_trait::async_trait;
use mithril_common::entities::Snapshot;
use thiserror::Error;

#[cfg(test)]
use mockall::automock;

/// SnapshotStore represents a snapshot store interactor
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SnapshotStore: Sync + Send {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String>;

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: String) -> Result<Option<Snapshot>, String>;

    /// Upload a snapshot & update the snapshot list
    async fn add_snapshot(&mut self, snapshot: Snapshot) -> Result<(), SnapshotStoreError>;
}

#[derive(Error, Debug)]
pub enum SnapshotStoreError {
    #[error("Error while adding new snapshot to GCP: `{0}`")]
    GcpError(String),
}
