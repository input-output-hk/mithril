#![allow(dead_code)]
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use async_trait::async_trait;
use slog::{debug, Logger};

use mithril_common::{
    digesters::{IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR},
    entities::{AncillaryLocation, CompressionAlgorithm, ImmutableFileNumber},
    logging::LoggerExtensions,
    StdResult,
};

use crate::{FileUploader, LocalUploader, Snapshotter};

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
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(AncillaryLocation::CloudStorage { uri })
    }
}

/// The [AncillaryArtifactBuilder] creates an ancillary archive from the cardano database directory (including ledger and volatile directories).
/// The archive is uploaded with the provided uploaders.
pub struct AncillaryArtifactBuilder {
    uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
    snapshotter: Arc<dyn Snapshotter>,
    compression_algorithm: CompressionAlgorithm,
    logger: Logger,
}

impl AncillaryArtifactBuilder {
    /// Creates a new [AncillaryArtifactBuilder].
    pub fn new(
        uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
        snapshotter: Arc<dyn Snapshotter>,
        compression_algorithm: CompressionAlgorithm,
        logger: Logger,
    ) -> Self {
        Self {
            uploaders,
            logger: logger.new_with_component_name::<Self>(),
            compression_algorithm,
            snapshotter,
        }
    }

    /// Returns the list of files and directories to include in the snapshot.
    /// The immutable file number is incremented by 1 to include the not yet finalized immutable file.
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
    fn create_ancillary_archive(
        &self,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<PathBuf> {
        debug!(
            self.logger,
            "Creating ancillary archive for immutable file number: {}", immutable_file_number
        );

        let paths_to_include = Self::get_files_and_directories_to_snapshot(immutable_file_number);

        let archive_name = format!(
            "ancillary.{}",
            self.compression_algorithm.tar_file_extension()
        );

        let snapshot = self
            .snapshotter
            .snapshot_subset(&archive_name, paths_to_include)
            .with_context(|| {
                format!(
                    "Failed to create snapshot for immutable file number: {}",
                    immutable_file_number
                )
            })?;

        debug!(
            self.logger,
            "Ancillary archive created at path: {:?}",
            snapshot.get_file_path()
        );

        Ok(snapshot.get_file_path().to_path_buf())
    }

    /// Uploads the ancillary archive and returns the locations of the uploaded files.
    pub async fn upload_archive(&self, db_directory: &Path) -> StdResult<Vec<AncillaryLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            // TODO: Temporary preparation work, `db_directory` is used as the ancillary archive path for now.
            let location = uploader.upload(db_directory).await?;
            locations.push(location);
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use flate2::read::GzDecoder;
    use mockall::predicate::eq;
    use tar::Archive;

    use mithril_common::digesters::{
        DummyCardanoDbBuilder, IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR,
    };

    use crate::{
        test_tools::TestLogger, CompressedArchiveSnapshotter, DumbSnapshotter,
        SnapshotterCompressionAlgorithm,
    };

    use super::*;

    #[tokio::test]
    async fn upload_archive_should_return_empty_locations_with_no_uploader() {
        let builder = AncillaryArtifactBuilder::new(
            vec![],
            Arc::new(DumbSnapshotter::new()),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        );

        let locations = builder.upload_archive(Path::new("whatever")).await.unwrap();

        assert!(locations.is_empty());
    }

    #[tokio::test]
    async fn upload_archive_should_return_all_uploaders_returned_locations() {
        let mut first_uploader = MockAncillaryFileUploader::new();
        first_uploader
            .expect_upload()
            .with(eq(Path::new("archive_path")))
            .times(1)
            .return_once(|_| {
                Ok(AncillaryLocation::CloudStorage {
                    uri: "an_uri".to_string(),
                })
            });

        let mut second_uploader = MockAncillaryFileUploader::new();
        second_uploader
            .expect_upload()
            .with(eq(Path::new("archive_path")))
            .times(1)
            .return_once(|_| {
                Ok(AncillaryLocation::CloudStorage {
                    uri: "another_uri".to_string(),
                })
            });

        let uploaders: Vec<Arc<dyn AncillaryFileUploader>> =
            vec![Arc::new(first_uploader), Arc::new(second_uploader)];

        let builder = AncillaryArtifactBuilder::new(
            uploaders,
            Arc::new(DumbSnapshotter::new()),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        );

        let locations = builder
            .upload_archive(Path::new("archive_path"))
            .await
            .unwrap();

        assert_eq!(
            locations,
            vec![
                AncillaryLocation::CloudStorage {
                    uri: "an_uri".to_string()
                },
                AncillaryLocation::CloudStorage {
                    uri: "another_uri".to_string()
                }
            ]
        );
    }

    #[tokio::test]
    async fn create_archive_should_embed_ledger_volatile_directories_and_last_immutables() {
        let test_dir = "cardano_database/create_archive";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2, 3])
            .with_ledger_files(&["blocks-0.dat", "blocks-1.dat", "blocks-2.dat"])
            .with_volatile_files(&["437", "537", "637", "737"])
            .build();
        std::fs::create_dir(cardano_db.get_dir().join("whatever")).unwrap();

        let db_directory = cardano_db.get_dir().to_path_buf();
        let snapshotter = {
            CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap()
        };

        let builder = AncillaryArtifactBuilder::new(
            vec![],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        );

        let archive_file_path = builder.create_ancillary_archive(1).unwrap();

        let mut archive = {
            let file_tar_gz = File::open(archive_file_path).unwrap();
            let file_tar_gz_decoder = GzDecoder::new(file_tar_gz);
            Archive::new(file_tar_gz_decoder)
        };

        let dst = cardano_db.get_dir().join("unpack_dir");
        archive.unpack(dst.clone()).unwrap();

        let expected_immutable_path = dst.join(IMMUTABLE_DIR);
        assert!(expected_immutable_path.join("00002.chunk").exists());
        assert!(expected_immutable_path.join("00002.primary").exists());
        assert!(expected_immutable_path.join("00002.secondary").exists());
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
}
