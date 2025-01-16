use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{
    entities::DigestLocation, logging::LoggerExtensions,
    messages::CardanoDatabaseDigestListItemMessage, StdResult,
};
use reqwest::Url;
use slog::{error, Logger};

use crate::ImmutableFileDigestMapper;

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

    immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,

    logger: Logger,
}

impl DigestArtifactBuilder {
    /// Creates a new [DigestArtifactBuilder].
    pub fn new(
        aggregator_url_prefix: Url,
        uploaders: Vec<Arc<dyn DigestFileUploader>>,
        immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,
        logger: Logger,
    ) -> StdResult<Self> {
        Ok(Self {
            aggregator_url_prefix,
            uploaders,
            immutable_file_digest_mapper,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    pub async fn upload(&self) -> StdResult<Vec<DigestLocation>> {
        let digest_path = self.create_digest_archive().await?;

        let locations = self.upload_digest_archive(&digest_path).await;
        fs::remove_file(&digest_path).with_context(|| {
            format!(
                "Could not remove digest archive file: '{}'",
                digest_path.display()
            )
        })?;
        locations
    }

    async fn create_digest_archive(&self) -> StdResult<PathBuf> {
        let immutable_file_digest_map = self
            .immutable_file_digest_mapper
            .get_immutable_file_digest_map()
            .await?
            .into_iter()
            .map(
                |(immutable_file_name, digest)| CardanoDatabaseDigestListItemMessage {
                    immutable_file_name,
                    digest,
                },
            )
            .collect::<Vec<_>>();

        // TODO : change that injecting the path or using snapshotter
        let digests_file_path = Path::new("/tmp").join("mithril").join("digests.json");

        if let Some(digests_dir) = digests_file_path.parent() {
            fs::create_dir_all(digests_dir).with_context(|| {
                format!(
                    "Can not create digests directory: '{}'",
                    digests_dir.display()
                )
            })?;
        }

        let digest_file = fs::File::create(digests_file_path.clone()).unwrap();
        serde_json::to_writer(digest_file, &immutable_file_digest_map)?;

        Ok(digests_file_path)
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
    use std::{collections::BTreeMap, fs::read_to_string};

    use crate::{
        immutable_file_digest_mapper::MockImmutableFileDigestMapper, test_tools::TestLogger,
    };
    use anyhow::anyhow;
    use mithril_common::{
        messages::{CardanoDatabaseDigestListItemMessage, CardanoDatabaseDigestListMessage},
        test_utils::{assert_equivalent, TempDir},
    };

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
        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            vec![],
            Arc::new(immutable_file_digest_mapper),
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
                Arc::new(MockImmutableFileDigestMapper::new()),
                TestLogger::file(&log_path),
            )
            .unwrap();

            let _ = builder
                .upload_digest_archive(Path::new("digest_file"))
                .await;
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
            Arc::new(MockImmutableFileDigestMapper::new()),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_digest_archive(Path::new("digest_file"))
            .await
            .unwrap();

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
            Arc::new(MockImmutableFileDigestMapper::new()),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_digest_archive(Path::new("digest_file"))
            .await
            .unwrap();

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
            Arc::new(MockImmutableFileDigestMapper::new()),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_digest_archive(Path::new("digest_file"))
            .await
            .unwrap();

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

    #[tokio::test]
    async fn create_digest_archive_should_create_json_file_with_all_digests() {
        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| {
                Ok(BTreeMap::from([(
                    "06685.chunk".to_string(),
                    "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e".to_string(),
                )]))
            });

        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            vec![],
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let archive_path = builder.create_digest_archive().await.unwrap();
        let file_content = read_to_string(archive_path).unwrap();
        let digest_content: CardanoDatabaseDigestListMessage =
            serde_json::from_str(&file_content).unwrap();

        assert_eq!(
            digest_content,
            vec![CardanoDatabaseDigestListItemMessage {
                immutable_file_name: "06685.chunk".to_string(),
                digest: "0af556ab2620dd9363bf76963a231abe8948a500ea6be31b131d87907ab09b1e"
                    .to_string(),
            }]
        );
    }

    #[tokio::test]
    async fn upload_should_call_upload_with_created_digest_file_and_delete_the_file() {
        // TODO : This test is flaky because we create and remove a file with an hard coded path
        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let mut digest_file_uploader = MockDigestFileUploader::new();
        digest_file_uploader
            .expect_upload()
            .withf(|path| path == Path::new("/tmp/mithril/digests.json") && path.exists())
            .times(1)
            .return_once(|_| {
                Ok(DigestLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                })
            });

        let builder = DigestArtifactBuilder::new(
            Url::parse("https://aggregator/").unwrap(),
            vec![Arc::new(digest_file_uploader)],
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let _locations = builder.upload().await.unwrap();

        assert!(!Path::new("/tmp/mithril/digests.json").exists());
    }
}
