use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog::{debug, error, Logger};

use mithril_common::{
    digesters::{IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR},
    entities::{AncillaryLocation, CardanoDbBeacon, CompressionAlgorithm},
    logging::LoggerExtensions,
    CardanoNetwork, StdResult,
};

use crate::{
    file_uploaders::LocalUploader, snapshotter::OngoingSnapshot, FileUploader, Snapshotter,
};

/// The [AncillaryFileUploader] trait allows identifying uploaders that return locations for ancillary archive files.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait AncillaryFileUploader: Send + Sync {
    /// Uploads the archive at the given filepath and returns the location of the uploaded file.
    async fn upload(&self, filepath: &Path) -> StdResult<AncillaryLocation>;
}

#[async_trait]
impl AncillaryFileUploader for LocalUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<AncillaryLocation> {
        let uri = FileUploader::upload(self, filepath)
            .await
            .with_context(|| "Error while uploading with 'LocalUploader'")?
            .into();

        Ok(AncillaryLocation::CloudStorage { uri })
    }
}

/// The [AncillaryArtifactBuilder] creates an ancillary archive from the cardano database directory (including ledger and volatile directories).
/// The archive is uploaded with the provided uploaders.
pub struct AncillaryArtifactBuilder {
    uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
    snapshotter: Arc<dyn Snapshotter>,
    cardano_network: CardanoNetwork,
    compression_algorithm: CompressionAlgorithm,
    logger: Logger,
}

impl AncillaryArtifactBuilder {
    /// Creates a new [AncillaryArtifactBuilder].
    pub fn new(
        uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
        snapshotter: Arc<dyn Snapshotter>,
        cardano_network: CardanoNetwork,
        compression_algorithm: CompressionAlgorithm,
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
            compression_algorithm,
            snapshotter,
        })
    }

    pub async fn upload(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<AncillaryLocation>> {
        let snapshot = self.create_ancillary_archive(beacon)?;

        let locations = self
            .upload_ancillary_archive(snapshot.get_file_path())
            .await?;

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
    fn create_ancillary_archive(&self, beacon: &CardanoDbBeacon) -> StdResult<OngoingSnapshot> {
        debug!(
            self.logger,
            "Creating ancillary archive for immutable file number: {}",
            beacon.immutable_file_number
        );

        let paths_to_include =
            Self::get_files_and_directories_to_snapshot(beacon.immutable_file_number);

        let archive_name = format!(
            "{}-e{}-i{}.ancillary.{}",
            self.cardano_network,
            *beacon.epoch,
            beacon.immutable_file_number,
            self.compression_algorithm.tar_file_extension()
        );

        let ancillary_archive_path = Path::new("cardano-database")
            .join("ancillary")
            .join(&archive_name);

        let snapshot = self
            .snapshotter
            .snapshot_subset(&ancillary_archive_path, paths_to_include)
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
        archive_filepath: &Path,
    ) -> StdResult<Vec<AncillaryLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            let result = uploader.upload(archive_filepath).await;
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

        if locations.is_empty() {
            return Err(anyhow!(
                "Failed to upload ancillary archive with all uploaders"
            ));
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use flate2::read::GzDecoder;
    use tar::Archive;

    use mithril_common::{
        digesters::{DummyCardanoDbBuilder, IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR},
        test_utils::{assert_equivalent, TempDir},
    };
    use uuid::Uuid;

    use crate::{
        test_tools::TestLogger, CompressedArchiveSnapshotter, DumbSnapshotter,
        SnapshotterCompressionAlgorithm,
    };

    use super::*;

    fn fake_uploader_returning_error() -> MockAncillaryFileUploader {
        let mut uploader = MockAncillaryFileUploader::new();
        uploader
            .expect_upload()
            .return_once(|_| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn fake_uploader(archive_path: &str, location_uri: &str) -> MockAncillaryFileUploader {
        let uri = location_uri.to_string();
        let filepath = archive_path.to_string();
        let mut uploader = MockAncillaryFileUploader::new();
        uploader
            .expect_upload()
            .withf(move |p| p == Path::new(&filepath))
            .times(1)
            .return_once(|_| Ok(AncillaryLocation::CloudStorage { uri }));

        uploader
    }

    #[test]
    fn create_ancillary_builder_should_error_when_no_uploader() {
        let result = AncillaryArtifactBuilder::new(
            vec![],
            Arc::new(DumbSnapshotter::new()),
            CardanoNetwork::DevNet(123),
            CompressionAlgorithm::Gzip,
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
            .return_once(|_| Err(anyhow!("Failure while uploading...")));

        {
            let builder = AncillaryArtifactBuilder::new(
                vec![Arc::new(uploader)],
                Arc::new(DumbSnapshotter::new()),
                CardanoNetwork::DevNet(123),
                CompressionAlgorithm::Gzip,
                TestLogger::file(&log_path),
            )
            .unwrap();

            let _ = builder
                .upload_ancillary_archive(Path::new("archive_path"))
                .await;
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();
        assert!(logs.contains("Failure while uploading..."));
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_error_when_no_location_is_returned() {
        let uploader = fake_uploader_returning_error();

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(DumbSnapshotter::new()),
            CardanoNetwork::DevNet(123),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let result = builder
            .upload_ancillary_archive(Path::new("archive_path"))
            .await;

        assert!(
            result.is_err(),
            "Should return an error when no location is returned"
        );
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_return_location_even_with_uploaders_errors() {
        let first_uploader = fake_uploader_returning_error();
        let second_uploader = fake_uploader("archive_path", "an_uri");
        let third_uploader = fake_uploader_returning_error();

        let uploaders: Vec<Arc<dyn AncillaryFileUploader>> = vec![
            Arc::new(first_uploader),
            Arc::new(second_uploader),
            Arc::new(third_uploader),
        ];

        let builder = AncillaryArtifactBuilder::new(
            uploaders,
            Arc::new(DumbSnapshotter::new()),
            CardanoNetwork::DevNet(123),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_ancillary_archive(Path::new("archive_path"))
            .await
            .unwrap();

        assert_equivalent(
            locations,
            vec![AncillaryLocation::CloudStorage {
                uri: "an_uri".to_string(),
            }],
        );
    }

    #[tokio::test]
    async fn upload_ancillary_archive_should_return_all_uploaders_returned_locations() {
        let first_uploader = fake_uploader("archive_path", "an_uri");
        let second_uploader = fake_uploader("archive_path", "another_uri");

        let uploaders: Vec<Arc<dyn AncillaryFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = AncillaryArtifactBuilder::new(
            uploaders,
            Arc::new(DumbSnapshotter::new()),
            CardanoNetwork::DevNet(123),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let locations = builder
            .upload_ancillary_archive(Path::new("archive_path"))
            .await
            .unwrap();

        assert_equivalent(
            locations,
            vec![
                AncillaryLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                },
                AncillaryLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                },
            ],
        );
    }

    #[tokio::test]
    async fn create_archive_should_embed_ledger_volatile_directories_and_last_immutables() {
        let test_dir = "create_archive/cardano_database";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2, 3])
            .with_ledger_files(&["blocks-0.dat", "blocks-1.dat", "blocks-2.dat"])
            .with_volatile_files(&["437", "537", "637", "737"])
            .build();
        std::fs::create_dir(cardano_db.get_dir().join("whatever")).unwrap();

        let db_directory = cardano_db.get_dir().to_path_buf();
        let mut snapshotter = CompressedArchiveSnapshotter::new(
            db_directory.clone(),
            db_directory.parent().unwrap().join("snapshot_dest"),
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();
        snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(MockAncillaryFileUploader::new())],
            Arc::new(snapshotter),
            CardanoNetwork::DevNet(123),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let snapshot = builder
            .create_ancillary_archive(&CardanoDbBeacon::new(99, 2))
            .unwrap();

        let mut archive = {
            let file_tar_gz = File::open(snapshot.get_file_path()).unwrap();
            let file_tar_gz_decoder = GzDecoder::new(file_tar_gz);
            Archive::new(file_tar_gz_decoder)
        };

        let dst = cardano_db.get_dir().join("unpack_dir");
        archive.unpack(dst.clone()).unwrap();

        let expected_immutable_path = dst.join(IMMUTABLE_DIR);
        assert!(expected_immutable_path.join("00003.chunk").exists());
        assert!(expected_immutable_path.join("00003.primary").exists());
        assert!(expected_immutable_path.join("00003.secondary").exists());
        let immutables_nb = std::fs::read_dir(expected_immutable_path).unwrap().count();
        assert_eq!(3, immutables_nb);

        let expected_ledger_path = dst.join(LEDGER_DIR);
        assert!(expected_ledger_path.join("blocks-0.dat").exists());
        assert!(expected_ledger_path.join("blocks-1.dat").exists());
        assert!(expected_ledger_path.join("blocks-2.dat").exists());
        let ledger_nb = std::fs::read_dir(expected_ledger_path).unwrap().count();
        assert_eq!(3, ledger_nb);

        let expected_volatile_path = dst.join(VOLATILE_DIR);
        assert!(expected_volatile_path.join("437").exists());
        assert!(expected_volatile_path.join("537").exists());
        assert!(expected_volatile_path.join("637").exists());
        assert!(expected_volatile_path.join("737").exists());
        let volatile_nb = std::fs::read_dir(expected_volatile_path).unwrap().count();
        assert_eq!(4, volatile_nb);

        assert!(!dst.join("whatever").exists());
    }

    #[tokio::test]
    async fn upload_should_return_error_and_not_upload_when_archive_creation_fails() {
        let mut snapshotter = CompressedArchiveSnapshotter::new(
            PathBuf::from("directory_not_existing"),
            PathBuf::from("whatever"),
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();
        snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

        let mut uploader = MockAncillaryFileUploader::new();
        uploader.expect_upload().never();

        let builder = AncillaryArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(snapshotter),
            CardanoNetwork::DevNet(123),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        builder
            .upload(&CardanoDbBeacon::new(99, 1))
            .await
            .expect_err("Should return an error when archive creation fails");
    }
}
