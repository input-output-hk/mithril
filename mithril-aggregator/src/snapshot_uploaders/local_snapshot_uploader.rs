use crate::http_server;
use crate::snapshot_uploaders::{SnapshotLocation, SnapshotUploader};
use crate::tools;

use async_trait::async_trait;
use slog_scope::debug;
use std::path::Path;

/// LocalSnapshotUploader is a snapshot uploader working using local files
pub struct LocalSnapshotUploader {
    /// Snapshot server listening IP
    snapshot_server_url: String,
}

impl LocalSnapshotUploader {
    /// LocalSnapshotUploader factory
    pub(crate) fn new(snapshot_server_url: String) -> Self {
        debug!("New LocalSnapshotUploader created"; "snapshot_server_url" => &snapshot_server_url);
        Self {
            snapshot_server_url,
        }
    }
}

#[async_trait]
impl SnapshotUploader for LocalSnapshotUploader {
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> Result<SnapshotLocation, String> {
        let archive_name = snapshot_filepath.file_name().unwrap().to_str().unwrap();
        let digest = tools::extract_digest_from_path(Path::new(archive_name));
        let location = format!(
            "http://{}/{}/snapshot/{}/download",
            self.snapshot_server_url,
            http_server::SERVER_BASE_PATH,
            digest.unwrap()
        );

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use super::LocalSnapshotUploader;
    use crate::http_server;
    use crate::snapshot_uploaders::SnapshotUploader;
    use std::path::Path;

    #[tokio::test]
    async fn should_extract_digest_to_deduce_location() {
        let url = "test.com:8080".to_string();
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        let snapshot_path = format!("testnet.{}.tar.gz", digest);
        let expected_location = format!(
            "http://{}/{}/snapshot/{}/download",
            url,
            http_server::SERVER_BASE_PATH,
            &digest
        );
        let uploader = LocalSnapshotUploader::new(url);

        assert_eq!(
            Ok(expected_location),
            uploader.upload_snapshot(Path::new(&snapshot_path)).await,
        );
    }
}
