use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{
    entities::{CardanoDbBeacon, CompressionAlgorithm, DigestLocation},
    logging::LoggerExtensions,
    messages::CardanoDatabaseDigestListItemMessage,
    CardanoNetwork, StdResult,
};
use slog::{error, Logger};

use crate::{
    file_uploaders::{GcpUploader, LocalUploader},
    services::Snapshotter,
    tools::url_sanitizer::SanitizedUrlWithTrailingSlash,
    DumbUploader, FileUploader, ImmutableFileDigestMapper,
};

/// The [DigestFileUploader] trait allows identifying uploaders that return locations for digest files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait DigestFileUploader: Send + Sync {
    /// Uploads the file at the given filepath and returns the location of the uploaded file.
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<DigestLocation>;
}

#[async_trait]
impl DigestFileUploader for DumbUploader {
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<DigestLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(DigestLocation::CloudStorage {
            uri,
            compression_algorithm,
        })
    }
}

#[async_trait]
impl DigestFileUploader for LocalUploader {
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<DigestLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(DigestLocation::CloudStorage {
            uri,
            compression_algorithm,
        })
    }
}

#[async_trait]
impl DigestFileUploader for GcpUploader {
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<DigestLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(DigestLocation::CloudStorage {
            uri,
            compression_algorithm,
        })
    }
}

pub struct DigestUpload {
    pub locations: Vec<DigestLocation>,
    pub size: u64,
}

pub struct DigestSnapshotter {
    pub snapshotter: Arc<dyn Snapshotter>,
    pub compression_algorithm: CompressionAlgorithm,
}

impl DigestSnapshotter {
    fn create_archive_file(&self, digest_file_name: PathBuf) -> Result<PathBuf, anyhow::Error> {
        let digest_archive_file_path =
            digest_file_name.with_extension(self.compression_algorithm.tar_file_extension());
        let ongoing_digests_archive = self
            .snapshotter
            .snapshot_subset(&digest_archive_file_path, vec![digest_file_name.clone()])
            .with_context(|| {
                format!(
                    "Could not create snapshot of digest file: '{}'",
                    digest_file_name.display()
                )
            })?;

        Ok(ongoing_digests_archive.get_file_path().clone())
    }
}

pub struct DigestArtifactBuilder {
    /// Aggregator URL prefix
    aggregator_url_prefix: SanitizedUrlWithTrailingSlash,

    /// Uploaders
    uploaders: Vec<Arc<dyn DigestFileUploader>>,

    digest_snapshotter: DigestSnapshotter,

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
        digest_snapshotter: DigestSnapshotter,
        network: CardanoNetwork,
        digests_dir: PathBuf,
        immutable_file_digest_mapper: Arc<dyn ImmutableFileDigestMapper>,
        logger: Logger,
    ) -> StdResult<Self> {
        Ok(Self {
            aggregator_url_prefix,
            uploaders,
            digest_snapshotter,
            network,
            digests_dir,
            immutable_file_digest_mapper,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    pub async fn upload(&self, beacon: &CardanoDbBeacon) -> StdResult<DigestUpload> {
        let digest_path = self.create_digest_file(beacon).await?;
        let digest_file_name =
            PathBuf::from(Self::get_digests_file_name(&self.network, beacon, "json"));
        let digest_archive_file_path = self
            .digest_snapshotter
            .create_archive_file(digest_file_name)?;

        let locations = self.upload_digest_file(&digest_archive_file_path).await;

        let file_metadata = std::fs::metadata(&digest_path);

        fs::remove_file(&digest_path).with_context(|| {
            format!("Could not remove digest file: '{}'", digest_path.display())
        })?;
        if digest_archive_file_path.exists() {
            fs::remove_file(&digest_archive_file_path).with_context(|| {
                format!(
                    "Could not remove digest archive file: '{}'",
                    digest_archive_file_path.display()
                )
            })?;
        }

        let size = file_metadata
            .with_context(|| {
                format!(
                    "Could not get size of digest file: '{}'",
                    digest_path.display()
                )
            })?
            .len();

        Ok(DigestUpload {
            locations: locations?,
            size,
        })
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

        let digests_file_path = DigestArtifactBuilder::get_digests_file_path(
            &self.digests_dir,
            &self.network,
            beacon,
            "json",
        );

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
            let result = uploader.upload(digest_filepath, None).await;
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

    fn get_digests_file_name(
        network: &CardanoNetwork,
        beacon: &CardanoDbBeacon,
        extension: &str,
    ) -> String {
        let filename = format!(
            "{}-e{}-i{}.digests.{}",
            network, *beacon.epoch, beacon.immutable_file_number, extension
        );
        filename
    }

    fn get_digests_file_path<P: AsRef<Path>>(
        digests_dir: P,
        network: &CardanoNetwork,
        beacon: &CardanoDbBeacon,
        extension: &str,
    ) -> PathBuf {
        let filename = Self::get_digests_file_name(network, beacon, extension);
        digests_dir.as_ref().join(filename)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeMap,
        fs::{read_to_string, File},
    };

    use crate::{
        immutable_file_digest_mapper::MockImmutableFileDigestMapper,
        services::{
            CompressedArchiveSnapshotter, DumbSnapshotter, SnapshotterCompressionAlgorithm,
        },
        test_tools::TestLogger,
    };
    use anyhow::anyhow;
    use flate2::read::GzDecoder;
    use mithril_common::{
        current_function,
        entities::{CardanoDbBeacon, CompressionAlgorithm},
        messages::{CardanoDatabaseDigestListItemMessage, CardanoDatabaseDigestListMessage},
        test_utils::{assert_equivalent, TempDir},
    };
    use mockall::predicate::eq;
    use tar::Archive;
    use uuid::Uuid;

    use super::*;

    fn fake_uploader_returning_error() -> MockDigestFileUploader {
        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_, _| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn fake_uploader(
        location_uri: &str,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> MockDigestFileUploader {
        let uri = location_uri.to_string();
        let mut uploader = MockDigestFileUploader::new();
        uploader.expect_upload().times(1).return_once(move |_, _| {
            Ok(DigestLocation::CloudStorage {
                uri,
                compression_algorithm,
            })
        });

        uploader
    }

    #[tokio::test]
    async fn digest_artifact_builder_return_digests_route_on_aggregator() {
        let temp_dir = TempDir::create("digest", current_function!());

        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![],
            DigestSnapshotter {
                snapshotter: Arc::new(DumbSnapshotter::new()),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
            CardanoNetwork::DevNet(123),
            temp_dir,
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let upload_info = builder.upload(&CardanoDbBeacon::new(4, 123)).await.unwrap();
        assert_eq!(
            vec!(DigestLocation::Aggregator {
                uri: "https://aggregator/artifact/cardano-database/digests".to_string()
            }),
            upload_info.locations
        );
    }

    #[tokio::test]
    async fn digest_artifact_builder_return_size_of_digest_file() {
        let temp_dir = TempDir::create("digest", current_function!());

        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![],
            DigestSnapshotter {
                snapshotter: Arc::new(DumbSnapshotter::new()),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
            CardanoNetwork::DevNet(123),
            temp_dir,
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let beacon = CardanoDbBeacon::new(4, 123);
        let upload_info = builder.upload(&beacon).await.unwrap();

        let digest_path = builder.create_digest_file(&beacon).await.unwrap();

        let expected_size = std::fs::metadata(digest_path).unwrap().len();
        assert!(expected_size > 0);
        assert_eq!(expected_size, upload_info.size);
    }

    #[tokio::test]
    async fn upload_digest_file_should_log_upload_errors() {
        let log_path = TempDir::create("digest", current_function!()).join("test.log");

        let mut uploader = MockDigestFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_, _| Err(anyhow!("Failure while uploading...")));

        {
            let builder = DigestArtifactBuilder::new(
                SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
                vec![Arc::new(uploader)],
                DigestSnapshotter {
                    snapshotter: Arc::new(DumbSnapshotter::new()),
                    compression_algorithm: CompressionAlgorithm::Gzip,
                },
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
            DigestSnapshotter {
                snapshotter: Arc::new(DumbSnapshotter::new()),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
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
        let second_uploader = fake_uploader("an_uri", Some(CompressionAlgorithm::Gzip));
        let third_uploader = fake_uploader_returning_error();

        let uploaders: Vec<Arc<dyn DigestFileUploader>> = vec![
            Arc::new(first_uploader),
            Arc::new(second_uploader),
            Arc::new(third_uploader),
        ];

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            uploaders,
            DigestSnapshotter {
                snapshotter: Arc::new(DumbSnapshotter::new()),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
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
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                },
                DigestLocation::Aggregator {
                    uri: "https://aggregator/artifact/cardano-database/digests".to_string(),
                },
            ],
        );
    }

    #[tokio::test]
    async fn upload_digest_file_should_return_all_uploaders_returned_locations() {
        let first_uploader = fake_uploader("an_uri", Some(CompressionAlgorithm::Gzip));
        let second_uploader = fake_uploader("another_uri", Some(CompressionAlgorithm::Gzip));

        let uploaders: Vec<Arc<dyn DigestFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            uploaders,
            DigestSnapshotter {
                snapshotter: Arc::new(DumbSnapshotter::new()),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
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
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                },
                DigestLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                },
                DigestLocation::Aggregator {
                    uri: "https://aggregator/artifact/cardano-database/digests".to_string(),
                },
            ],
        );
    }

    #[tokio::test]
    async fn create_digest_file_should_create_json_file_with_all_digests() {
        let temp_dir = TempDir::create("digest", current_function!());
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
            DigestSnapshotter {
                snapshotter: Arc::new(DumbSnapshotter::new()),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
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
    async fn upload_should_call_upload_with_created_digest_archive_file_and_delete_the_file() {
        let tmp_dir = TempDir::create("digest", current_function!());
        let digests_dir = tmp_dir.join("digest");
        let digests_archive_dir = tmp_dir.join("archive");
        let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();
        immutable_file_digest_mapper
            .expect_get_immutable_file_digest_map()
            .returning(|| Ok(BTreeMap::new()));

        let mut digest_file_uploader = MockDigestFileUploader::new();

        let compression_algorithm = CompressionAlgorithm::Gzip;

        let beacon = CardanoDbBeacon::new(3, 456);
        let network = CardanoNetwork::DevNet(24);
        let digest_file_name =
            DigestArtifactBuilder::get_digests_file_name(&network, &beacon, "json");
        let archive_path = DigestArtifactBuilder::get_digests_file_path(
            &digests_archive_dir,
            &network,
            &beacon,
            &compression_algorithm.tar_file_extension(),
        );

        digest_file_uploader
            .expect_upload()
            .with(eq(archive_path.clone()), eq(None))
            .times(1)
            .return_once(move |_, _| {
                assert!(
                    archive_path.exists(),
                    "Path to upload should exist: {}",
                    archive_path.display()
                );

                let unpack_dir = tmp_dir.join("unpack");
                unpack_archive(&archive_path, &unpack_dir);

                let unpack_digest_file = unpack_dir.join(digest_file_name);
                assert!(unpack_digest_file.is_file());

                Ok(DigestLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                    compression_algorithm: None,
                })
            });

        let mut snapshotter = CompressedArchiveSnapshotter::new(
            digests_dir.clone(),
            digests_archive_dir,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();
        snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

        let builder = DigestArtifactBuilder::new(
            SanitizedUrlWithTrailingSlash::parse("https://aggregator/").unwrap(),
            vec![Arc::new(digest_file_uploader)],
            DigestSnapshotter {
                snapshotter: Arc::new(snapshotter),
                compression_algorithm: CompressionAlgorithm::Gzip,
            },
            network,
            digests_dir.clone(),
            Arc::new(immutable_file_digest_mapper),
            TestLogger::stdout(),
        )
        .unwrap();

        let _locations = builder.upload(&beacon).await.unwrap();

        let remaining_files: Vec<_> = std::fs::read_dir(&digests_dir)
            .unwrap()
            .map(|res| res.unwrap().path())
            .collect();
        assert!(
            remaining_files.is_empty(),
            "There should be no remaining files, but found: {:?}",
            remaining_files
        );
    }

    fn unpack_archive(archive_path: &Path, unpack_dir: &Path) {
        let mut archive = {
            let file_tar_gz = File::open(archive_path).unwrap();
            let file_tar_gz_decoder = GzDecoder::new(file_tar_gz);
            Archive::new(file_tar_gz_decoder)
        };

        archive.unpack(unpack_dir).unwrap();
    }

    #[tokio::test]
    async fn get_digest_file_path_include_beacon_information() {
        let digests_dir = TempDir::create("digest", current_function!());

        let beacon = CardanoDbBeacon::new(5, 456);
        let network = CardanoNetwork::MainNet;
        let digest_file =
            DigestArtifactBuilder::get_digests_file_path(&digests_dir, &network, &beacon, "json");

        assert_eq!(
            digest_file,
            digests_dir.join("mainnet-e5-i456.digests.json")
        );
    }
}
