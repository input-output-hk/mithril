use crate::snapshot_uploaders::{SnapshotLocation, SnapshotUploader};

use async_trait::async_trait;
use slog_scope::debug;
use std::path::Path;

/// LocalSnapshotUploader is a snapshot uploader working using local files
pub struct LocalSnapshotUploader {}

impl Default for LocalSnapshotUploader {
    /// LocalSnapshotUploader factory
    fn default() -> Self {
        debug!("New LocalSnapshotUploader created");
        Self {}
    }
}

#[async_trait]
impl SnapshotUploader for LocalSnapshotUploader {
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> Result<SnapshotLocation, String> {
        let archive_name = snapshot_filepath.file_name().unwrap().to_str().unwrap();
        let digest = archive_name.split('.').nth(1);
        let location = format!("https://0.0.0.0/snapshot/{}/download", &digest.unwrap());

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use super::LocalSnapshotUploader;
    use crate::snapshot_uploaders::SnapshotUploader;
    use std::path::Path;

    #[tokio::test]
    async fn should_extract_digest_to_deduce_location() {
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        let snapshot_path = format!("testnet.{}.tar.gz", digest);
        let expected_location = format!("https://0.0.0.0/snapshot/{}/download", &digest);
        let uploader = LocalSnapshotUploader::default();

        assert_eq!(
            Ok(expected_location),
            uploader.upload_snapshot(Path::new(&snapshot_path)).await,
        );
    }
}
