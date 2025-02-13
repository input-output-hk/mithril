use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{
    entities::{CardanoDbBeacon, DigestLocation},
    logging::LoggerExtensions,
    messages::CardanoDatabaseDigestListItemMessage,
    CardanoNetwork, StdResult,
};
use slog::{error, Logger};

use crate::{
    file_uploaders::{GcpUploader, LocalUploader},
    tools::url_sanitizer::SanitizedUrlWithTrailingSlash,
    DumbUploader, FileUploader, ImmutableFileDigestMapper,
};

/// The [DigestFileUploader] trait allows identifying uploaders that return locations for digest files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait DigestFileUploader: Send + Sync {
    /// Uploads the file at the given filepath and returns the location of the uploaded file.
    async fn upload(&self, filepath: &Path) -> StdResult<DigestLocation>;
}

#[async_trait]
impl DigestFileUploader for DumbUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<DigestLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(DigestLocation::CloudStorage { uri })
    }
}

#[async_trait]
impl DigestFileUploader for LocalUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<DigestLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(DigestLocation::CloudStorage { uri })
    }
}

#[async_trait]
impl DigestFileUploader for GcpUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<DigestLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(DigestLocation::CloudStorage { uri })
    }
}

pub struct DigestArtifactBuilder {
    /// Aggregator URL prefix
    aggregator_url_prefix: SanitizedUrlWithTrailingSlash,

    /// Uploaders
    uploaders: Vec<Arc<dyn DigestFileUploader>>,

    network: CardanoNetwork,

    digests_dir: PathBuf,

    immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,

    logger: Logger,
}

impl DigestArtifactBuilder {
    /// Creates a new [DigestArtifactBuilder].
    pub fn new(
        aggregator_url_prefix: SanitizedUrlWithTrailingSlash,
        uploaders: Vec<Arc<dyn DigestFileUploader>>,
        network: CardanoNetwork,
        digests_dir: PathBuf,
        immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,
        logger: Logger,
    ) -> StdResult<Self> {
        Ok(Self {
            aggregator_url_prefix,
            uploaders,
            network,
            digests_dir,
            immutable_file_digest_mapper,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    pub async fn upload(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<DigestLocation>> {
        let digest_path = self.create_digest_file(beacon).await?;

        let locations = self.upload_digest_file(&digest_path).await;
        fs::remove_file(&digest_path).with_context(|| {
            format!("Could not remove digest file: '{}'", digest_path.display())
        })?;

        locations
    }

    async fn create_digest_file(&self, beacon: &CardanoDbBeacon) -> StdResult<PathBuf> {
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

        let digests_file_path =
            DigestArtifactBuilder::get_digests_file_path(&self.digests_dir, &self.network, beacon);

        if let Some(digests_dir) = digests_file_path.parent() {
            fs::create_dir_all(digests_dir).with_context(|| {
                format!(
                    "Can not create digests directory: '{}'",
                    digests_dir.display()
                )
            })?;
        }

        let digest_file = fs::File::create(digests_file_path.clone())?;
        serde_json::to_writer(digest_file, &immutable_file_digest_map)?;

        Ok(digests_file_path)
    }

    /// Uploads the digest file and returns the locations of the uploaded files.
    async fn upload_digest_file(&self, digest_filepath: &Path) -> StdResult<Vec<DigestLocation>> {
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
                        "Failed to upload digest file";
                        "error" => e.to_string()
                    );
                }
            }
        }

        locations.push(self.aggregator_digests_route_location()?);

        Ok(locations)
    }

    fn aggregator_digests_route_location(&self) -> StdResult<DigestLocation> {
        Ok(DigestLocation::Aggregator {
            uri: self
                .aggregator_url_prefix
                .join("artifact/cardano-database/digests")?
                .to_string(),
        })
    }

    fn get_digests_file_path<P: AsRef<Path>>(
        digests_dir: P,
        network: &CardanoNetwork,
        beacon: &CardanoDbBeacon,
    ) -> PathBuf {
        let filename = format!(
            "{}-e{}-i{}.digests.json",
            network, *beacon.epoch, beacon.immutable_file_number
        );
        digests_dir.as_ref().join(filename)
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
        entities::CardanoDbBeacon,
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
        let temp_dir = TempDir::create(
            "digest",
            "digest_artifact_builder_return_digests_route_on_aggregator",
        );
        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![],
            CardanoNetwork::DevNet(123),
            temp_dir,
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder.upload(&CardanoDbBeacon::new(4, 123)).await.unwrap();
        assert_eq!(
            vec!(DigestLocation::Aggregator {
                uri: "https://aggregator/artifact/cardano-database/digests".to_string()
            }),
            locations
        );
    }

    #[tokio::test]
    async fn upload_digest_file_should_log_upload_errors() {
        let log_path = TempDir::create("digest", "upload_digest_file_should_log_upload_errors")
            .join("test.log");

        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_| Err(anyhow!("Failure while uploading...")));

        {
            let builder = DigestArtifactBuilder::new(
                SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
                vec![Arc::new(uploader)],
                CardanoNetwork::DevNet(123),
                PathBuf::from("/tmp/whatever"),
                Arc::new(MockImmutableFileDigestMapper::new()),
                TestLogger::file(&log_path),
            )
            .unwrap();

            let _ = builder.upload_digest_file(Path::new("digest_file")).await;
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(logs.contains("Failure while uploading..."));
    }

    #[tokio::test]
    async fn upload_digest_file_should_not_error_even_if_no_location_returned_from_uploaders() {
        let uploader = fake_uploader_returning_error();

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![Arc::new(uploader)],
            CardanoNetwork::DevNet(123),
            PathBuf::from("/tmp/whatever"),
            Arc::new(MockImmutableFileDigestMapper::new()),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_digest_file(Path::new("digest_file"))
            .await
            .unwrap();

        assert!(!locations.is_empty());
    }

    #[tokio::test]
    async fn upload_digest_file_should_return_location_even_with_uploaders_errors() {
        let first_uploader = fake_uploader_returning_error();
        let second_uploader = fake_uploader("an_uri");
        let third_uploader = fake_uploader_returning_error();

        let uploaders: Vec<Arc<dyn DigestFileUploader>> = vec![
            Arc::new(first_uploader),
            Arc::new(second_uploader),
            Arc::new(third_uploader),
        ];

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            uploaders,
            CardanoNetwork::DevNet(123),
            PathBuf::from("/tmp/whatever"),
            Arc::new(MockImmutableFileDigestMapper::new()),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_digest_file(Path::new("digest_file"))
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
    async fn upload_digest_file_should_return_all_uploaders_returned_locations() {
        let first_uploader = fake_uploader("an_uri");
        let second_uploader = fake_uploader("another_uri");

        let uploaders: Vec<Arc<dyn DigestFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            uploaders,
            CardanoNetwork::DevNet(123),
            PathBuf::from("/tmp/whatever"),
            Arc::new(MockImmutableFileDigestMapper::new()),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_digest_file(Path::new("digest_file"))
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
    async fn create_digest_file_should_create_json_file_with_all_digests() {
        let temp_dir = TempDir::create(
            "digest",
            "create_digest_file_should_create_json_file_with_all_digests",
        );
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
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![],
            CardanoNetwork::DevNet(123),
            temp_dir,
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let digest_file = builder
            .create_digest_file(&CardanoDbBeacon::new(4, 123))
            .await
            .unwrap();

        let file_content = read_to_string(digest_file).unwrap();
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
        let digests_dir = TempDir::create(
            "digests",
            "upload_should_call_upload_with_created_digest_file_and_delete_the_file",
        );
        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let mut digest_file_uploader = MockDigestFileUploader::new();

        let beacon = CardanoDbBeacon::new(3, 456);
        let network = CardanoNetwork::DevNet(24);
        let digest_file =
            DigestArtifactBuilder::get_digests_file_path(&digests_dir, &network, &beacon);

        let digest_file_clone = digest_file.clone();
        digest_file_uploader
            .expect_upload()
            .withf(move |path| path == digest_file_clone && path.exists())
            .times(1)
            .return_once(|_| {
                Ok(DigestLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                })
            });

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![Arc::new(digest_file_uploader)],
            network,
            digests_dir,
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let _locations = builder.upload(&beacon).await.unwrap();

        assert!(!digest_file.exists());
    }

    #[tokio::test]
    async fn get_digest_file_path_include_beacon_information() {
        let digests_dir =
            TempDir::create("digests", "get_digest_file_path_include_beacon_information");

        let beacon = CardanoDbBeacon::new(5, 456);
        let network = CardanoNetwork::MainNet;
        let digest_file =
            DigestArtifactBuilder::get_digests_file_path(&digests_dir, &network, &beacon);

        assert_eq!(
            digest_file,
            digests_dir.join("mainnet-e5-i456.digests.json")
        );
    }
}
