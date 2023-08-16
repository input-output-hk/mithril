use async_trait::async_trait;
use mithril_common::StdResult;
use std::path::Path;

#[cfg(test)]
use mockall::automock;

pub type SnapshotLocation = String;

/// SnapshotUploader represents a snapshot uploader interactor
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SnapshotUploader: Sync + Send {
    /// Upload a snapshot
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> StdResult<SnapshotLocation>;
}
