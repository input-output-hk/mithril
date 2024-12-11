use async_trait::async_trait;
use slog::{debug, Logger};
use std::path::Path;

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::file_uploaders::{FileLocation, FileUploader};

/// RemoteUploader is a snapshot uploader working using Google Cloud Platform services
pub struct RemoteUploader {
    file_uploader: Box<dyn FileUploader>,
    logger: Logger,
}

impl RemoteUploader {
    /// RemoteUploader factory
    pub fn new(file_uploader: Box<dyn FileUploader>, logger: Logger) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New RemoteUploader created");
        Self {
            file_uploader,
            logger,
        }
    }
}

#[async_trait]
impl FileUploader for RemoteUploader {
    async fn upload(&self, snapshot_filepath: &Path) -> StdResult<FileLocation> {
        let location = self.file_uploader.upload(snapshot_filepath).await?;
        debug!(self.logger, "Snapshot upload to remote storage completed"; "location" => &location);

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mockall::predicate::eq;
    use std::path::Path;

    use crate::file_uploaders::{FileUploader, MockFileUploader};
    use crate::test_tools::TestLogger;

    use super::RemoteUploader;

    #[tokio::test]
    async fn upload_call_uploader_and_return_location() {
        let mut file_uploader = MockFileUploader::new();
        file_uploader
            .expect_upload()
            .with(eq(Path::new("test/snapshot.xxx.tar.gz")))
            .times(1)
            .returning(|_| Ok("https://cdn.mithril.network/snapshot.xxx.tar.gz".to_string()));
        let snapshot_uploader = RemoteUploader::new(Box::new(file_uploader), TestLogger::stdout());
        let filepath = Path::new("test/snapshot.xxx.tar.gz");
        let expected_location = "https://cdn.mithril.network/snapshot.xxx.tar.gz".to_string();

        let location = snapshot_uploader
            .upload(filepath)
            .await
            .expect("remote upload should not fail");

        assert_eq!(expected_location, location);
    }

    #[tokio::test]
    async fn upload_return_error_when_uploader_error() {
        let mut file_uploader = MockFileUploader::new();
        file_uploader
            .expect_upload()
            .returning(|_| Err(anyhow!("unexpected error")));
        let snapshot_uploader = RemoteUploader::new(Box::new(file_uploader), TestLogger::stdout());
        let snapshot_filepath = Path::new("test/snapshot.xxx.tar.gz");

        let result = snapshot_uploader
            .upload(snapshot_filepath)
            .await
            .expect_err("remote upload should fail");
        assert_eq!("unexpected error".to_string(), result.to_string());
    }
}
