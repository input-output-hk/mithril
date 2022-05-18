use crate::snapshot_uploaders::{SnapshotLocation, SnapshotUploader};
use crate::tools::GcpFileUploader;
use std::path::Path;

use async_trait::async_trait;
use slog_scope::debug;

/// GCPSnapshotUploader is a snapshot uploader working using Google Cloud Platform services
pub struct GCPSnapshotUploader {}

impl GCPSnapshotUploader {
    /// GCPSnapshotUploader factory
    pub fn new() -> Self {
        debug!("New GCPSnapshotUploader created");
        Self {}
    }
}

#[async_trait]
impl SnapshotUploader for GCPSnapshotUploader {
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> Result<SnapshotLocation, String> {
        let archive_name = snapshot_filepath.file_name().unwrap().to_str().unwrap();
        let location = format!(
            "https://storage.googleapis.com/cardano-testnet/{}",
            archive_name
        );

        GcpFileUploader::default()
            .upload_file(snapshot_filepath)
            .await?;

        Ok(location)
    }
}

#[cfg(test)]
mod tests {}
