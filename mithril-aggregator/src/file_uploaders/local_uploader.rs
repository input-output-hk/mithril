use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};
use std::path::{Path, PathBuf};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::file_uploaders::{FileUploader, FileUri};
use crate::http_server;
use crate::tools;

/// LocalUploader is a file uploader working using local files
pub struct LocalUploader {
    /// File server listening IP
    file_server_url: String,

    /// Target folder where to store files archive
    target_location: PathBuf,

    logger: Logger,
}

impl LocalUploader {
    /// LocalUploader factory
    pub(crate) fn new(file_server_url: String, target_location: &Path, logger: Logger) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New LocalUploader created"; "file_server_url" => &file_server_url);
        Self {
            file_server_url,
            target_location: target_location.to_path_buf(),
            logger,
        }
    }
}

#[async_trait]
impl FileUploader for LocalUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri> {
        let archive_name = filepath.file_name().unwrap().to_str().unwrap();
        let target_path = &self.target_location.join(archive_name);
        tokio::fs::copy(filepath, target_path)
            .await
            .with_context(|| "File copy failure")?;

        let digest = tools::extract_digest_from_path(Path::new(archive_name));
        let specific_route = "artifact/snapshot";
        let location = format!(
            "{}{}/{}/{}/download",
            self.file_server_url,
            http_server::SERVER_BASE_PATH,
            specific_route,
            digest.unwrap()
        );

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

    use crate::file_uploaders::{FileUploader, FileUri};
    use crate::http_server;
    use crate::test_tools::TestLogger;

    use super::LocalUploader;

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
        let uploader = LocalUploader::new(url, target_dir.path(), TestLogger::stdout());

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
        let uploader = LocalUploader::new(
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

    #[tokio::test]
    async fn should_error_if_path_is_a_directory() {
        let source_dir = tempdir().unwrap();
        let digest = "41e27b9ed5a32531b95b2b7ff3c0757591a06a337efaf19a524a998e348028e7";
        create_fake_archive(source_dir.path(), digest);
        let target_dir = tempdir().unwrap();
        let uploader = LocalUploader::new(
            "http://test.com:8080/".to_string(),
            target_dir.path(),
            TestLogger::stdout(),
        );
        uploader
            .upload(source_dir.path())
            .await
            .expect_err("Uploading a directory should fail");
    }
}
