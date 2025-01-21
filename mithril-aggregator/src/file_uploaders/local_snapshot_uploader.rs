use anyhow::Context;
use async_trait::async_trait;
use mithril_common::entities::FileUri;
use reqwest::Url;
use slog::{debug, Logger};
use std::path::{Path, PathBuf};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::file_uploaders::FileUploader;
use crate::tools::{self, url_sanitizer::sanitize_url_path};

// It's only used by the legacy snapshot that uploads the entire Cardano database.
/// LocalSnapshotUploader is a file uploader working using local files
pub struct LocalSnapshotUploader {
    /// File server URL prefix
    server_url_prefix: Url,

    /// Target folder where to store files archive
    target_location: PathBuf,

    logger: Logger,
}

impl LocalSnapshotUploader {
    /// LocalSnapshotUploader factory
    pub(crate) fn new(
        server_url_prefix: Url,
        target_location: &Path,
        logger: Logger,
    ) -> StdResult<Self> {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New LocalSnapshotUploader created"; "server_url_prefix" => &server_url_prefix.as_str());
        let server_url_prefix = sanitize_url_path(&server_url_prefix)?;

        Ok(Self {
            server_url_prefix,
            target_location: target_location.to_path_buf(),
            logger,
        })
    }
}

#[async_trait]
impl FileUploader for LocalSnapshotUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri> {
        let archive_name = filepath.file_name().unwrap().to_str().unwrap();
        let target_path = &self.target_location.join(archive_name);
        tokio::fs::copy(filepath, target_path)
            .await
            .with_context(|| "File copy failure")?;

        let digest = tools::extract_digest_from_path(Path::new(archive_name))?;
        let location = &self
            .server_url_prefix
            .join("artifact/snapshot/")?
            .join(&format!("{digest}/"))?
            .join("download")?;
        let location = location.as_str().to_string();

        debug!(self.logger, "File 'uploaded' to local storage"; "location" => &location);
        Ok(FileUri(location))
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Write;
    use std::path::{Path, PathBuf};
    use tempfile::tempdir;

    use crate::file_uploaders::FileUploader;
    use crate::test_tools::TestLogger;

    use super::*;

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
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        let archive = create_fake_archive(source_dir.path(), digest);
        let expected_location = format!(
            "http://test.com:8080/base-root/artifact/snapshot/{}/download",
            &digest
        );

        let url_prefix = Url::parse("http://test.com:8080/base-root").unwrap();
        let uploader =
            LocalSnapshotUploader::new(url_prefix, target_dir.path(), TestLogger::stdout())
                .unwrap();
        let location = uploader
            .upload(&archive)
            .await
            .expect("local upload should not fail");

        assert_eq!(FileUri(expected_location), location);
    }

    #[tokio::test]
    async fn should_copy_file_to_target_location() {
        let source_dir = tempdir().unwrap();
        let target_dir = tempdir().unwrap();
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        let archive = create_fake_archive(source_dir.path(), digest);
        let uploader = LocalSnapshotUploader::new(
            Url::parse("http://test.com:8080/base-root/").unwrap(),
            target_dir.path(),
            TestLogger::stdout(),
        )
        .unwrap();
        uploader.upload(&archive).await.unwrap();

        assert!(target_dir
            .path()
            .join(archive.file_name().unwrap())
            .exists());
    }

    #[tokio::test]
    async fn should_error_if_path_is_a_directory() {
        let source_dir = tempdir().unwrap();
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        create_fake_archive(source_dir.path(), digest);
        let target_dir = tempdir().unwrap();
        let uploader = LocalSnapshotUploader::new(
            Url::parse("http://test.com:8080/base-root/").unwrap(),
            target_dir.path(),
            TestLogger::stdout(),
        )
        .unwrap();
        uploader
            .upload(source_dir.path())
            .await
            .expect_err("Uploading a directory should fail");
    }
}
