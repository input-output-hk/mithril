use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};
use std::path::{Path, PathBuf};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::file_uploaders::{FileLocation, FileUploader};
use crate::http_server;
use crate::tools;

/// LocalSnapshotUploader is a snapshot uploader working using local files
pub struct LocalSnapshotUploader {
    /// Snapshot server listening IP
    snapshot_server_url: String,

    /// Target folder where to store snapshots archive
    target_location: PathBuf,

    logger: Logger,
}

impl LocalSnapshotUploader {
    /// LocalSnapshotUploader factory
    pub(crate) fn new(snapshot_server_url: String, target_location: &Path, logger: Logger) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New LocalSnapshotUploader created"; "snapshot_server_url" => &snapshot_server_url);
        Self {
            snapshot_server_url,
            target_location: target_location.to_path_buf(),
            logger,
        }
    }
}

#[async_trait]
impl FileUploader for LocalSnapshotUploader {
    async fn upload(&self, snapshot_filepath: &Path) -> StdResult<FileLocation> {
        let archive_name = snapshot_filepath.file_name().unwrap().to_str().unwrap();
        let target_path = &self.target_location.join(archive_name);
        tokio::fs::copy(snapshot_filepath, target_path)
            .await
            .with_context(|| "Snapshot copy failure")?;

        let digest = tools::extract_digest_from_path(Path::new(archive_name));
        let location = format!(
            "{}{}/artifact/snapshot/{}/download",
            self.snapshot_server_url,
            http_server::SERVER_BASE_PATH,
            digest.unwrap()
        );

        debug!(self.logger, "Snapshot 'uploaded' to local storage"; "location" => &location);
        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Write;
    use std::path::{Path, PathBuf};
    use tempfile::tempdir;

    use crate::file_uploaders::FileUploader;
    use crate::http_server;
    use crate::test_tools::TestLogger;

    use super::LocalSnapshotUploader;

    fn create_fake_archive(dir: &Path, digest: &str) -> PathBuf {
        let file_path = dir.join(format!("test.{digest}.tar.gz"));
        let mut file = File::create(&file_path).unwrap();
        writeln!(
            file,
            "I swear, this is an archive, not a temporary test file."
        )
        .unwrap();

        file_path
    }

    #[tokio::test]
    async fn should_extract_digest_to_deduce_location() {
        let source_dir = tempdir().unwrap();
        let target_dir = tempdir().unwrap();
        let url = "http://test.com:8080/".to_string();
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        let archive = create_fake_archive(source_dir.path(), digest);
        let expected_location = format!(
            "{}{}/artifact/snapshot/{}/download",
            url,
            http_server::SERVER_BASE_PATH,
            &digest
        );
        let uploader = LocalSnapshotUploader::new(url, target_dir.path(), TestLogger::stdout());

        let location = uploader
            .upload(&archive)
            .await
            .expect("local upload should not fail");

        assert_eq!(expected_location, location);
    }

    #[tokio::test]
    async fn should_copy_file_to_target_location() {
        let source_dir = tempdir().unwrap();
        let target_dir = tempdir().unwrap();
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        let archive = create_fake_archive(source_dir.path(), digest);
        let uploader = LocalSnapshotUploader::new(
            "http://test.com:8080/".to_string(),
            target_dir.path(),
            TestLogger::stdout(),
        );
        uploader.upload(&archive).await.unwrap();

        assert!(target_dir
            .path()
            .join(archive.file_name().unwrap())
            .exists());
    }
}
