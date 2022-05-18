use async_trait::async_trait;
use mithril_common::entities::Snapshot;
use reqwest::{self, StatusCode};
use slog_scope::debug;

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
    async fn upload_snapshot(&mut self) -> Result<(), String>;
}
