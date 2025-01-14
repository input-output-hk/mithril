use std::sync::Arc;

use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::{entities::DigestLocation, logging::LoggerExtensions, StdResult};
use slog::{error, Logger};

/// The [DigestFileUploader] trait allows identifying uploaders that return locations for digest archive files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait DigestFileUploader: Send + Sync {
    /// Uploads the archive at the given filepath and returns the location of the uploaded file.
    async fn upload(&self) -> StdResult<DigestLocation>;
}

pub struct DigestArtifactBuilder {
    uploaders: Vec<Arc<dyn DigestFileUploader>>,
    logger: Logger,
}

impl DigestArtifactBuilder {
    /// Creates a new [DigestArtifactBuilder].
    pub fn new(uploaders: Vec<Arc<dyn DigestFileUploader>>, logger: Logger) -> StdResult<Self> {
        if uploaders.is_empty() {
            return Err(anyhow!(
                "At least one uploader is required to create an 'DigestArtifactBuilder'"
            ));
        }

        Ok(Self {
            uploaders,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    pub async fn upload(&self) -> StdResult<Vec<DigestLocation>> {
        self.upload_digest_archive().await
    }

    /// Uploads the digest archive and returns the locations of the uploaded files.
    async fn upload_digest_archive(&self) -> StdResult<Vec<DigestLocation>> {
        let mut locations = Vec::<DigestLocation>::new();
        for uploader in &self.uploaders {
            let result = uploader.upload().await;
            match result {
                Ok(location) => {
                    locations.push(location);
                }
                Err(e) => {
                    error!(
                        self.logger,
                        "Failed to upload digest archive";
                        "error" => e.to_string()
                    );
                }
            }
        }

        if locations.is_empty() {
            return Err(anyhow!(
                "Failed to upload digest archive with all uploaders"
            ));
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use crate::test_tools::TestLogger;
    use mithril_common::test_utils::{assert_equivalent, TempDir};

    use super::*;

    fn fake_uploader_returning_error() -> MockDigestFileUploader {
        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn fake_uploader(location_uri: &str) -> MockDigestFileUploader {
        let uri = location_uri.to_string();
        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .times(1)
            .return_once(|| Ok(DigestLocation::CloudStorage { uri }));

        uploader
    }

    #[test]
    fn create_digest_builder_should_error_when_no_uploader() {
        let result = DigestArtifactBuilder::new(vec![], TestLogger::stdout());

        assert!(result.is_err(), "Should return an error when no uploaders")
    }

    #[tokio::test]
    async fn upload_digest_archive_should_log_upload_errors() {
        let log_path = TempDir::create("digest", "upload_digest_archive_should_log_upload_errors")
            .join("test.log");

        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|| Err(anyhow!("Failure while uploading...")));

        {
            let builder =
                DigestArtifactBuilder::new(vec![Arc::new(uploader)], TestLogger::file(&log_path))
                    .unwrap();

            let _ = builder.upload_digest_archive().await;
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(logs.contains("Failure while uploading..."));
    }

    #[tokio::test]
    async fn upload_digest_archive_should_error_when_no_location_is_returned() {
        let uploader = fake_uploader_returning_error();

        let builder =
            DigestArtifactBuilder::new(vec![Arc::new(uploader)], TestLogger::stdout()).unwrap();

        let result = builder.upload_digest_archive().await;

        assert!(
            result.is_err(),
            "Should return an error when no location is returned"
        );
    }

    #[tokio::test]
    async fn upload_digest_archive_should_return_location_even_with_uploaders_errors() {
        let first_uploader = fake_uploader_returning_error();
        let second_uploader = fake_uploader("an_uri");
        let third_uploader = fake_uploader_returning_error();

        let uploaders: Vec<Arc<dyn DigestFileUploader>> = vec![
            Arc::new(first_uploader),
            Arc::new(second_uploader),
            Arc::new(third_uploader),
        ];

        let builder = DigestArtifactBuilder::new(uploaders, TestLogger::stdout()).unwrap();

        let locations = builder.upload_digest_archive().await.unwrap();

        assert_equivalent(
            locations,
            vec![DigestLocation::CloudStorage {
                uri: "an_uri".to_string(),
            }],
        );
    }

    #[tokio::test]
    async fn upload_digest_archive_should_return_all_uploaders_returned_locations() {
        let first_uploader = fake_uploader("an_uri");
        let second_uploader = fake_uploader("another_uri");

        let uploaders: Vec<Arc<dyn DigestFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = DigestArtifactBuilder::new(uploaders, TestLogger::stdout()).unwrap();

        let locations = builder.upload_digest_archive().await.unwrap();

        assert_equivalent(
            locations,
            vec![
                DigestLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                },
                DigestLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                },
            ],
        );
    }
}
