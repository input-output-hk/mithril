use async_trait::async_trait;
use slog::{debug, Logger};
use std::path::Path;

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::file_uploaders::{SnapshotLocation, SnapshotUploader};
use crate::tools::RemoteFileUploader;

/// GCPSnapshotUploader is a snapshot uploader working using Google Cloud Platform services
pub struct RemoteSnapshotUploader {
    bucket: String,
    file_uploader: Box<dyn RemoteFileUploader>,
    use_cdn_domain: bool,
    logger: Logger,
}

impl RemoteSnapshotUploader {
    /// GCPSnapshotUploader factory
    pub fn new(
        file_uploader: Box<dyn RemoteFileUploader>,
        bucket: String,
        use_cdn_domain: bool,
        logger: Logger,
    ) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New GCPSnapshotUploader created");
        Self {
            bucket,
            file_uploader,
            use_cdn_domain,
            logger,
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

        debug!(self.logger, "Uploading snapshot to remote storage"; "location" => &location);
        self.file_uploader.upload_file(snapshot_filepath).await?;
        debug!(self.logger, "Snapshot upload to remote storage completed"; "location" => &location);

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::path::Path;

    use crate::file_uploaders::SnapshotUploader;
    use crate::test_tools::TestLogger;
    use crate::tools::MockRemoteFileUploader;

    use super::RemoteSnapshotUploader;

    #[tokio::test]
    async fn test_upload_snapshot_not_using_cdn_domain_ok() {
        let use_cdn_domain = false;
        let mut file_uploader = MockRemoteFileUploader::new();
        file_uploader.expect_upload_file().returning(|_| Ok(()));
        let snapshot_uploader = RemoteSnapshotUploader::new(
            Box::new(file_uploader),
            "cardano-testnet".to_string(),
            use_cdn_domain,
            TestLogger::stdout(),
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
            TestLogger::stdout(),
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
        let snapshot_uploader = RemoteSnapshotUploader::new(
            Box::new(file_uploader),
            "".to_string(),
            false,
            TestLogger::stdout(),
        );
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");

        let result = snapshot_uploader
            .upload_snapshot(snapshot_filepath)
            .await
            .expect_err("remote upload should fail");
        assert_eq!("unexpected error".to_string(), result.to_string());
    }
}
