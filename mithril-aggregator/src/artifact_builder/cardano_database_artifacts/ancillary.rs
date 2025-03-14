use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog::{debug, error, warn, Logger};

use mithril_common::{
    digesters::{IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR},
    entities::{AncillaryLocation, CardanoDbBeacon, CompressionAlgorithm},
    logging::LoggerExtensions,
    CardanoNetwork, StdResult,
};

use crate::{
    file_uploaders::{GcpUploader, LocalUploader},
    services::Snapshotter,
    tools::file_archiver::FileArchive,
    tools::file_size,
    DumbUploader, FileUploader,
};

/// The [AncillaryFileUploader] trait allows identifying uploaders that return locations for ancillary archive files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AncillaryFileUploader: Send + Sync {
    /// Uploads the archive at the given filepath and returns the location of the uploaded file.
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<AncillaryLocation>;
}

#[async_trait]
impl AncillaryFileUploader for DumbUploader {
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<AncillaryLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(AncillaryLocation::CloudStorage {
            uri,
            compression_algorithm,
        })
    }
}

#[async_trait]
impl AncillaryFileUploader for LocalUploader {
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<AncillaryLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(AncillaryLocation::CloudStorage {
            uri,
            compression_algorithm,
        })
    }
}

#[async_trait]
impl AncillaryFileUploader for GcpUploader {
    async fn upload(
        &self,
        filepath: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<AncillaryLocation> {
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(AncillaryLocation::CloudStorage {
            uri,
            compression_algorithm,
        })
    }
}

/// The [AncillaryArtifactBuilder] creates an ancillary archive from the cardano database directory (including ledger and volatile directories).
/// The archive is uploaded with the provided uploaders.
pub struct AncillaryArtifactBuilder {
    uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
    snapshotter: Arc<dyn Snapshotter>,
    cardano_network: CardanoNetwork,
    logger: Logger,
}

impl AncillaryArtifactBuilder {
    /// Creates a new [AncillaryArtifactBuilder].
    pub fn new(
        uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
        snapshotter: Arc<dyn Snapshotter>,
        cardano_network: CardanoNetwork,
        logger: Logger,
    ) -> StdResult<Self> {
        if uploaders.is_empty() {
            return Err(anyhow!(
                "At least one uploader is required to create an 'AncillaryArtifactBuilder'"
            ));
        }

        Ok(Self {
            uploaders,
            logger: logger.new_with_component_name::<Self>(),
            cardano_network,
            snapshotter,
        })
    }

    pub async fn upload(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<AncillaryLocation>> {
        let snapshot = self.create_ancillary_archive(beacon)?;
        let locations = self.upload_ancillary_archive(snapshot).await?;

        Ok(locations)
    }

    /// Returns the list of files and directories to include in the snapshot.
    /// The immutable file included in the ancillary archive corresponds to the last one (and not finalized yet)
    /// when the immutable file number given to the function corresponds to the penultimate.
    fn get_files_and_directories_to_snapshot(immutable_file_number: u64) -> Vec<PathBuf> {
        let next_immutable_file_number = immutable_file_number + 1;
        let chunk_filename = format!("{:05}.chunk", next_immutable_file_number);
        let primary_filename = format!("{:05}.primary", next_immutable_file_number);
        let secondary_filename = format!("{:05}.secondary", next_immutable_file_number);

        vec![
            PathBuf::from(VOLATILE_DIR),
            PathBuf::from(LEDGER_DIR),
            PathBuf::from(IMMUTABLE_DIR).join(chunk_filename),
            PathBuf::from(IMMUTABLE_DIR).join(primary_filename),
            PathBuf::from(IMMUTABLE_DIR).join(secondary_filename),
        ]
    }

    /// Creates an archive for the Cardano database ancillary files for the given immutable file number.
    fn create_ancillary_archive(&self, beacon: &CardanoDbBeacon) -> StdResult<FileArchive> {
        debug!(
            self.logger,
            "Creating ancillary archive for immutable file number: {}",
            beacon.immutable_file_number
        );

        let archive_name = format!(
            "{}-e{}-i{}.ancillary",
            self.cardano_network, *beacon.epoch, beacon.immutable_file_number,
        );

        let snapshot = self
            .snapshotter
            .snapshot_ancillary(beacon.immutable_file_number, &archive_name)
            .with_context(|| {
                format!(
                    "Failed to create ancillary archive for immutable file number: {}",
                    beacon.immutable_file_number
                )
            })?;

        debug!(
            self.logger,
            "Ancillary archive created at path: {:?}",
            snapshot.get_file_path()
        );

        Ok(snapshot)
    }

    /// Uploads the ancillary archive and returns the locations of the uploaded files.
    async fn upload_ancillary_archive(
        &self,
        file_archive: FileArchive,
    ) -> StdResult<Vec<AncillaryLocation>> {
        let archive_filepath = file_archive.get_file_path();
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            let result = uploader
                .upload(
                    archive_filepath,
                    Some(file_archive.get_compression_algorithm()),
                )
                .await;
            match result {
                Ok(location) => {
                    locations.push(location);
                }
                Err(e) => {
                    error!(
                        self.logger,
                        "Failed to upload ancillary archive";
                        "error" => e.to_string()
                    );
                }
            }
        }

        if let Err(error) = tokio::fs::remove_file(archive_filepath).await {
            warn!(
                self.logger, " > Post upload ancillary archive file removal failure";
                "error" => error
            );
        }

        if locations.is_empty() {
            return Err(anyhow!(
                "Failed to upload ancillary archive with all uploaders"
            ));
        }

        Ok(locations)
    }

    pub fn compute_uncompressed_size(
        &self,
        db_path: &Path,
        beacon: &CardanoDbBeacon,
    ) -> StdResult<u64> {
        let paths_to_include =
            Self::get_files_and_directories_to_snapshot(beacon.immutable_file_number);

        file_size::compute_size(
            paths_to_include
                .iter()
                .map(|path| db_path.join(path))
                .collect(),
        )
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        digesters::DummyCardanoDbBuilder,
        test_utils::{assert_equivalent, TempDir},
    };

    use crate::services::{DumbSnapshotter, MockSnapshotter};
    use crate::test_tools::TestLogger;

    use super::*;

    fn fake_uploader_returning_error() -> MockAncillaryFileUploader {
        let mut uploader = MockAncillaryFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_, _| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn fake_uploader(
        archive_path: &str,
        location_uri: &str,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> MockAncillaryFileUploader {
        let uri = location_uri.to_string();
        let filepath = archive_path.to_string();
        let mut uploader = MockAncillaryFileUploader::new();
        uploader
            .expect_upload()
            .withf(move |path, algorithm| {
                path == Path::new(&filepath) && algorithm == &compression_algorithm
            })
            .times(1)
            .return_once(move |_, _| {
                Ok(AncillaryLocation::CloudStorage {
                    uri,
                    compression_algorithm,
                })
            });

        uploader
    }

    fn create_fake_archive(dir: &Path, name: &str) -> PathBuf {
        use std::fs::File;
        use std::io::Write;

        let file_path = dir.join(name);
        let mut file = File::create(&file_path).unwrap();
        writeln!(
            file,
            "I swear, this is an archive, not a temporary test file."
        )
        .unwrap();

        file_path
    }

    #[test]
    fn create_ancillary_builder_should_error_when_no_uploader() {
        let result = AncillaryArtifactBuilder::new(
            vec![],
            Arc::new(DumbSnapshotter::default()),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        );

        assert!(result.is_err(), "Should return an error when no uploaders")
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_log_upload_errors() {
        let log_path = TempDir::create(
            "ancillary",
            "upload_ancillary_archive_should_log_upload_errors",
        )
        .join("test.log");

        let mut uploader = MockAncillaryFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_, _| Err(anyhow!("Failure while uploading...")));

        {
            let builder = AncillaryArtifactBuilder::new(
                vec![Arc::new(uploader)],
                Arc::new(DumbSnapshotter::default()),
                CardanoNetwork::DevNet(123),
                TestLogger::file(&log_path),
            )
            .unwrap();

            let _ = builder.upload_ancillary_archive(FileArchive::dummy()).await;
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(logs.contains("Failure while uploading..."));
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_error_when_no_location_is_returned() {
        let uploader = fake_uploader_returning_error();

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(DumbSnapshotter::default()),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        let result = builder.upload_ancillary_archive(FileArchive::dummy()).await;

        assert!(
            result.is_err(),
            "Should return an error when no location is returned"
        );
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_return_location_even_with_uploaders_errors() {
        let first_uploader = fake_uploader_returning_error();
        let second_uploader =
            fake_uploader("archive_path", "an_uri", Some(CompressionAlgorithm::Gzip));
        let third_uploader = fake_uploader_returning_error();

        let uploaders: Vec<Arc<dyn AncillaryFileUploader>> = vec![
            Arc::new(first_uploader),
            Arc::new(second_uploader),
            Arc::new(third_uploader),
        ];

        let builder = AncillaryArtifactBuilder::new(
            uploaders,
            Arc::new(DumbSnapshotter::default()),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_ancillary_archive(FileArchive::new(
                PathBuf::from("archive_path"),
                0,
                CompressionAlgorithm::Gzip,
            ))
            .await
            .unwrap();

        assert_equivalent(
            locations,
            vec![AncillaryLocation::CloudStorage {
                uri: "an_uri".to_string(),
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            }],
        );
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_return_all_uploaders_returned_locations() {
        let first_uploader =
            fake_uploader("archive_path", "an_uri", Some(CompressionAlgorithm::Gzip));
        let second_uploader = fake_uploader(
            "archive_path",
            "another_uri",
            Some(CompressionAlgorithm::Gzip),
        );

        let uploaders: Vec<Arc<dyn AncillaryFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = AncillaryArtifactBuilder::new(
            uploaders,
            Arc::new(DumbSnapshotter::new(CompressionAlgorithm::Gzip)),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_ancillary_archive(FileArchive::new(
                PathBuf::from("archive_path"),
                0,
                CompressionAlgorithm::Gzip,
            ))
            .await
            .unwrap();

        assert_equivalent(
            locations,
            vec![
                AncillaryLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                },
                AncillaryLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                },
            ],
        );
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_remove_archive_after_upload() {
        let source_dir = TempDir::create(
            "ancillary",
            "upload_ancillary_archive_should_remove_archive_after_upload",
        );
        let archive_path = create_fake_archive(&source_dir, "ancillary.tar.gz");
        let archive = FileArchive::new(archive_path.clone(), 0, CompressionAlgorithm::Gzip);
        let uploader = fake_uploader(
            archive_path.as_os_str().to_str().unwrap(),
            "an_uri",
            Some(CompressionAlgorithm::Gzip),
        );

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(DumbSnapshotter::new(CompressionAlgorithm::Gzip)),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        assert!(archive_path.exists());

        builder.upload_ancillary_archive(archive).await.unwrap();

        assert!(!archive_path.exists());
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_remove_archive_when_no_uploader_succeed() {
        let source_dir = TempDir::create(
            "ancillary",
            "upload_ancillary_archive_should_remove_archive_when_no_uploader_succeed",
        );
        let archive_path = create_fake_archive(&source_dir, "ancillary.tar.gz");
        let archive = FileArchive::new(archive_path.clone(), 0, CompressionAlgorithm::Gzip);
        let uploader = fake_uploader_returning_error();

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(DumbSnapshotter::default()),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        assert!(archive_path.exists());

        builder.upload_ancillary_archive(archive).await.unwrap_err();

        assert!(!archive_path.exists());
    }

    #[tokio::test]
    async fn upload_should_return_error_and_not_upload_when_archive_creation_fails() {
        let mut snapshotter = MockSnapshotter::new();
        snapshotter
            .expect_snapshot_ancillary()
            .returning(|_, _| Err(anyhow!("Failed to create archive")));

        let mut uploader = MockAncillaryFileUploader::new();
        uploader.expect_upload().never();

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(snapshotter),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        builder
            .upload(&CardanoDbBeacon::new(99, 1))
            .await
            .expect_err("Should return an error when archive creation fails");
    }

    #[test]
    fn should_compute_the_size_of_the_ancillary() {
        let test_dir = "should_compute_the_size_of_the_ancillary/cardano_database";

        let immutable_trio_file_size = 777;
        let ledger_file_size = 6666;
        let volatile_file_size = 99;

        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2, 3])
            .set_immutable_trio_file_size(immutable_trio_file_size)
            .with_ledger_files(&["437", "537", "637", "737"])
            .set_ledger_file_size(ledger_file_size)
            .with_volatile_files(&["blocks-0.dat", "blocks-1.dat", "blocks-2.dat"])
            .set_volatile_file_size(volatile_file_size)
            .build();

        let db_directory = cardano_db.get_dir().to_path_buf();

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(MockAncillaryFileUploader::new())],
            Arc::new(DumbSnapshotter::default()),
            CardanoNetwork::DevNet(123),
            TestLogger::stdout(),
        )
        .unwrap();

        let expected_total_size =
            immutable_trio_file_size + (4 * ledger_file_size) + (3 * volatile_file_size);

        let total_size = builder
            .compute_uncompressed_size(&db_directory, &CardanoDbBeacon::new(99, 1))
            .unwrap();

        assert_eq!(expected_total_size, total_size);
    }
}
