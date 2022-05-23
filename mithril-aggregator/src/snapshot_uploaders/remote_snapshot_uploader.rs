use crate::snapshot_uploaders::{SnapshotLocation, SnapshotUploader};
use crate::tools::RemoteFileUploader;
use std::path::Path;

use async_trait::async_trait;
use slog_scope::debug;

/// GCPSnapshotUploader is a snapshot uploader working using Google Cloud Platform services
pub struct RemoteSnapshotUploader {
    file_uploader: Box<dyn RemoteFileUploader>,
}

impl RemoteSnapshotUploader {
    /// GCPSnapshotUploader factory
    pub fn new(file_uploader: Box<dyn RemoteFileUploader>) -> Self {
        debug!("New GCPSnapshotUploader created");
        Self { file_uploader }
    }
}

#[async_trait]
impl SnapshotUploader for RemoteSnapshotUploader {
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> Result<SnapshotLocation, String> {
        let archive_name = snapshot_filepath.file_name().unwrap().to_str().unwrap();
        let location = format!(
            "https://storage.googleapis.com/cardano-testnet/{}",
            archive_name
        );

        self.file_uploader.upload_file(snapshot_filepath).await?;

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use super::RemoteSnapshotUploader;
    use crate::snapshot_uploaders::SnapshotUploader;
    use crate::tools::MockRemoteFileUploader;
    use std::path::Path;

    #[tokio::test]
    async fn test_upload_snapshot_ok() {
        let mut file_uploader = MockRemoteFileUploader::new();
        file_uploader.expect_upload_file().return_const(Ok(()));
        let snapshot_uploader = RemoteSnapshotUploader::new(Box::new(file_uploader));
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");
        let expected_location =
            "https://storage.googleapis.com/cardano-testnet/snapshot.xxx.tar.gz".to_string();

        assert_eq!(
            Ok(expected_location),
            snapshot_uploader.upload_snapshot(snapshot_filepath).await
        );
    }

    #[tokio::test]
    async fn test_upload_snapshot_ko() {
        let mut file_uploader = MockRemoteFileUploader::new();
        file_uploader
            .expect_upload_file()
            .return_const(Err("unexpected error".to_string()));
        let snapshot_uploader = RemoteSnapshotUploader::new(Box::new(file_uploader));
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");

        let result = snapshot_uploader.upload_snapshot(snapshot_filepath).await;
        assert_eq!(Err("unexpected error".to_string()), result);
    }
}
