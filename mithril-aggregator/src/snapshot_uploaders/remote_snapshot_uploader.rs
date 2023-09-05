use async_trait::async_trait;
use mithril_common::StdResult;
use slog_scope::debug;
use std::path::Path;

use crate::snapshot_uploaders::{SnapshotLocation, SnapshotUploader};
use crate::tools::RemoteFileUploader;

/// GCPSnapshotUploader is a snapshot uploader working using Google Cloud Platform services
pub struct RemoteSnapshotUploader {
    bucket: String,
    file_uploader: Box<dyn RemoteFileUploader>,
    use_cdn_domain: bool,
}

impl RemoteSnapshotUploader {
    /// GCPSnapshotUploader factory
    pub fn new(
        file_uploader: Box<dyn RemoteFileUploader>,
        bucket: String,
        use_cdn_domain: bool,
    ) -> Self {
        debug!("New GCPSnapshotUploader created");
        Self {
            bucket,
            file_uploader,
            use_cdn_domain,
        }
    }
}

#[async_trait]
impl SnapshotUploader for RemoteSnapshotUploader {
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> StdResult<SnapshotLocation> {
        let archive_name = snapshot_filepath.file_name().unwrap().to_str().unwrap();
        let location = if self.use_cdn_domain {
            format!("https://{}/{}", self.bucket, archive_name)
        } else {
            format!(
                "https://storage.googleapis.com/{}/{}",
                self.bucket, archive_name
            )
        };

        self.file_uploader.upload_file(snapshot_filepath).await?;

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use super::RemoteSnapshotUploader;
    use crate::snapshot_uploaders::SnapshotUploader;
    use crate::tools::MockRemoteFileUploader;
    use anyhow::anyhow;
    use std::path::Path;

    #[tokio::test]
    async fn test_upload_snapshot_not_using_cdn_domain_ok() {
        let use_cdn_domain = false;
        let mut file_uploader = MockRemoteFileUploader::new();
        file_uploader.expect_upload_file().returning(|_| Ok(()));
        let snapshot_uploader = RemoteSnapshotUploader::new(
            Box::new(file_uploader),
            "cardano-testnet".to_string(),
            use_cdn_domain,
        );
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");
        let expected_location =
            "https://storage.googleapis.com/cardano-testnet/snapshot.xxx.tar.gz".to_string();

        let location = snapshot_uploader
            .upload_snapshot(snapshot_filepath)
            .await
            .expect("remote upload should not fail");

        assert_eq!(expected_location, location);
    }

    #[tokio::test]
    async fn test_upload_snapshot_using_cdn_domain_ok() {
        let use_cdn_domain = true;
        let mut file_uploader = MockRemoteFileUploader::new();
        file_uploader.expect_upload_file().returning(|_| Ok(()));
        let snapshot_uploader = RemoteSnapshotUploader::new(
            Box::new(file_uploader),
            "cdn.mithril.network".to_string(),
            use_cdn_domain,
        );
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");
        let expected_location = "https://cdn.mithril.network/snapshot.xxx.tar.gz".to_string();

        let location = snapshot_uploader
            .upload_snapshot(snapshot_filepath)
            .await
            .expect("remote upload should not fail");

        assert_eq!(expected_location, location);
    }

    #[tokio::test]
    async fn test_upload_snapshot_ko() {
        let mut file_uploader = MockRemoteFileUploader::new();
        file_uploader
            .expect_upload_file()
            .returning(|_| Err(anyhow!("unexpected error")));
        let snapshot_uploader =
            RemoteSnapshotUploader::new(Box::new(file_uploader), "".to_string(), false);
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");

        let result = snapshot_uploader
            .upload_snapshot(snapshot_filepath)
            .await
            .expect_err("remote upload should fail");
        assert_eq!("unexpected error".to_string(), result.to_string());
    }
}
