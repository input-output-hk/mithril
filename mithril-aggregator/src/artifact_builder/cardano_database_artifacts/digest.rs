use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::{entities::DigestLocation, logging::LoggerExtensions, StdResult};
use reqwest::Url;
use slog::{debug, error, Logger};

use crate::{file_uploaders::url_sanitizer::sanitize_url_path, snapshotter::OngoingSnapshot};

/// The [DigestFileUploader] trait allows identifying uploaders that return locations for digest archive files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait DigestFileUploader: Send + Sync {
    /// Uploads the archive at the given filepath and returns the location of the uploaded file.
    async fn upload(&self, filepath: &Path) -> StdResult<DigestLocation>;
}

pub struct DigestArtifactBuilder {
    /// Aggregator URL prefix
    aggregator_url_prefix: Url,
    /// Uploaders
    uploaders: Vec<Arc<dyn DigestFileUploader>>,
    logger: Logger,
}

impl DigestArtifactBuilder {
    /// Creates a new [DigestArtifactBuilder].
    pub fn new(
        aggregator_url_prefix: Url,
        uploaders: Vec<Arc<dyn DigestFileUploader>>,
        logger: Logger,
    ) -> StdResult<Self> {
        Ok(Self {
            aggregator_url_prefix,
            uploaders,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    pub async fn upload(&self) -> StdResult<Vec<DigestLocation>> {
        // let snapshot = self.create_digest_archive(beacon)?;
        // let digest_path = snapshot.get_file_path();
        let digest_path = Path::new("");

        self.upload_digest_archive(digest_path).await
    }

    async fn create_digest_archive() -> StdResult<OngoingSnapshot> {
        // get message service::get_cardano_database_digest_list_message
        // output json (created from message) to file
        todo!()
    }

    /// Uploads the digest archive and returns the locations of the uploaded files.
    async fn upload_digest_archive(
        &self,
        digest_filepath: &Path,
    ) -> StdResult<Vec<DigestLocation>> {
        let mut locations = Vec::<DigestLocation>::new();
        for uploader in &self.uploaders {
            let result = uploader.upload(digest_filepath).await;
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

        locations.push(self.aggregator_location()?);

        Ok(locations)
    }

    fn aggregator_location(&self) -> StdResult<DigestLocation> {
        Ok(DigestLocation::Aggregator {
            uri: self
                .aggregator_url_prefix
                .join("artifact/cardano-database/digests")?
                .to_string(),
        })
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
            .return_once(|_| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn fake_uploader(location_uri: &str) -> MockDigestFileUploader {
        let uri = location_uri.to_string();
        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .times(1)
            .return_once(|_| Ok(DigestLocation::CloudStorage { uri }));

        uploader
    }

    #[tokio::test]
    async fn digest_artifact_builder_return_digests_route_on_aggregator() {
        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            vec![],
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder.upload().await.unwrap();
        assert_eq!(
            vec!(DigestLocation::Aggregator {
                uri: "https://aggregator/artifact/cardano-database/digests".to_string()
            }),
            locations
        );
    }

    #[tokio::test]
    async fn upload_digest_archive_should_log_upload_errors() {
        let log_path = TempDir::create("digest", "upload_digest_archive_should_log_upload_errors")
            .join("test.log");

        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_| Err(anyhow!("Failure while uploading...")));

        {
            let builder = DigestArtifactBuilder::new(
                Url::parse("https://aggregator/").unwrap(),
                vec![Arc::new(uploader)],
                TestLogger::file(&log_path),
            )
            .unwrap();

            let _ = builder.upload_digest_archive(&Path::new("")).await;
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(logs.contains("Failure while uploading..."));
    }

    #[tokio::test]
    async fn upload_digest_archive_should_not_error_even_if_no_location_returned_from_uploaders() {
        let uploader = fake_uploader_returning_error();

        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            vec![Arc::new(uploader)],
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder.upload_digest_archive(&Path::new("")).await.unwrap();

        assert!(!locations.is_empty());
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

        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            uploaders,
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder.upload_digest_archive(&Path::new("")).await.unwrap();

        assert_equivalent(
            locations,
            vec![
                DigestLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                },
                DigestLocation::Aggregator {
                    uri: "https://aggregator/artifact/cardano-database/digests".to_string(),
                },
            ],
        );
    }

    #[tokio::test]
    async fn upload_digest_archive_should_return_all_uploaders_returned_locations() {
        let first_uploader = fake_uploader("an_uri");
        let second_uploader = fake_uploader("another_uri");

        let uploaders: Vec<Arc<dyn DigestFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            uploaders,
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder.upload_digest_archive(&Path::new("")).await.unwrap();

        assert_equivalent(
            locations,
            vec![
                DigestLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                },
                DigestLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                },
                DigestLocation::Aggregator {
                    uri: "https://aggregator/artifact/cardano-database/digests".to_string(),
                },
            ],
        );
    }
}
