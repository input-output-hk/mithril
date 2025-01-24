use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};
use std::path::{Path, PathBuf};

use mithril_common::StdResult;
use mithril_common::{entities::FileUri, logging::LoggerExtensions};

use crate::file_uploaders::FileUploader;
use crate::tools::url_sanitizer::SanitizedUrlWithTrailingSlash;

/// LocalUploader is a file uploader working using local files
pub struct LocalUploader {
    /// File server URL prefix
    server_url_prefix: SanitizedUrlWithTrailingSlash,

    /// Target folder where to store files archive
    target_location: PathBuf,

    logger: Logger,
}

impl LocalUploader {
    /// LocalUploader factory
    pub(crate) fn new(
        server_url_prefix: SanitizedUrlWithTrailingSlash,
        target_location: &Path,
        logger: Logger,
    ) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        debug!(logger, "New LocalUploader created"; "server_url_prefix" => &server_url_prefix.as_str());

        Self {
            server_url_prefix,
            target_location: target_location.to_path_buf(),
            logger,
        }
    }
}

#[async_trait]
impl FileUploader for LocalUploader {
    async fn upload_without_retry(&self, filepath: &Path) -> StdResult<FileUri> {
        let archive_name = filepath.file_name().unwrap().to_str().unwrap();
        let target_path = &self.target_location.join(archive_name);
        tokio::fs::copy(filepath, target_path)
            .await
            .with_context(|| "File copy failure")?;

        let location = &self.server_url_prefix.join(archive_name)?;
        let location = location.as_str().to_string();

        debug!(
            self.logger, "File 'uploaded' to local storage";
            "location" => &location, "disk_path" => target_path.display()
        );
        Ok(FileUri(location))
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Write;
    use std::path::{Path, PathBuf};

    use mithril_common::test_utils::TempDir;

    use crate::test_tools::TestLogger;

    use super::*;

    fn create_fake_archive(dir: &Path, name: &str) -> PathBuf {
        let file_path = dir.join(format!("{name}.tar.gz"));
        let mut file = File::create(&file_path).unwrap();
        writeln!(
            file,
            "I swear, this is an archive, not a temporary test file."
        )
        .unwrap();

        file_path
    }

    #[tokio::test]
    async fn should_extract_archive_name_to_deduce_location() {
        let source_dir = TempDir::create(
            "local_uploader",
            "should_extract_archive_name_to_deduce_location_source",
        );
        let target_dir = TempDir::create(
            "local_uploader",
            "should_extract_archive_name_to_deduce_location_target",
        );
        let archive_name = "an_archive";
        let archive = create_fake_archive(&source_dir, archive_name);
        let expected_location = format!(
            "http://test.com:8080/base-root/{}",
            &archive.file_name().unwrap().to_str().unwrap()
        );

        let url_prefix =
            SanitizedUrlWithTrailingSlash::parse("http://test.com:8080/base-root").unwrap();
        let uploader = LocalUploader::new(url_prefix, &target_dir, TestLogger::stdout());
        let location = FileUploader::upload(&uploader, &archive)
            .await
            .expect("local upload should not fail");

        assert_eq!(FileUri(expected_location), location);
    }

    #[tokio::test]
    async fn should_copy_file_to_target_location() {
        let source_dir = TempDir::create(
            "local_uploader",
            "should_copy_file_to_target_location_source",
        );
        let target_dir = TempDir::create(
            "local_uploader",
            "should_copy_file_to_target_location_target",
        );
        println!("target_dir: {:?}", target_dir);
        let archive = create_fake_archive(&source_dir, "an_archive");
        let uploader = LocalUploader::new(
            SanitizedUrlWithTrailingSlash::parse("http://test.com:8080/base-root/").unwrap(),
            &target_dir,
            TestLogger::stdout(),
        );
        FileUploader::upload(&uploader, &archive).await.unwrap();

        assert!(target_dir.join(archive.file_name().unwrap()).exists());
    }

    #[tokio::test]
    async fn should_error_if_path_is_a_directory() {
        let source_dir = TempDir::create(
            "local_uploader",
            "should_error_if_path_is_a_directory_source",
        );
        let target_dir = TempDir::create(
            "local_uploader",
            "should_error_if_path_is_a_directory_target",
        );
        let uploader = LocalUploader::new(
            SanitizedUrlWithTrailingSlash::parse("http://test.com:8080/base-root/").unwrap(),
            &target_dir,
            TestLogger::stdout(),
        );
        FileUploader::upload(&uploader, &source_dir)
            .await
            .expect_err("Uploading a directory should fail");
    }
}
