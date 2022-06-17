use async_trait::async_trait;
use thiserror::Error;

use mithril_common::entities::Snapshot;

#[cfg(test)]
use mockall::automock;

/// SnapshotStore represents a snapshot store interactor
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SnapshotStore: Sync + Send {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, SnapshotStoreError>;

    /// Get snapshot details
    async fn get_snapshot_details(
        &self,
        digest: String,
    ) -> Result<Option<Snapshot>, SnapshotStoreError>;

    /// Upload a snapshot & update the snapshot list
    async fn add_snapshot(&mut self, snapshot: Snapshot) -> Result<(), SnapshotStoreError>;
}

#[derive(Error, Debug, Eq, PartialEq, Clone)]
pub enum SnapshotStoreError {
    #[error("Error while adding new snapshot to GCP: `{0}`")]
    Gcp(String),

    #[error("Manifest file error: `{0}`")]
    Manifest(String),

    #[error("Store error: `{0}`")]
    Store(String),
}
