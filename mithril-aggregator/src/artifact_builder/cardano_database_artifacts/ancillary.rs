#![allow(dead_code)]
use async_trait::async_trait;
use slog::Logger;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use mithril_common::{
    entities::{AncillaryLocation, ImmutableFileNumber},
    StdResult,
};

use crate::{
    CompressedArchiveSnapshotter, FileUploader, LocalUploader, Snapshotter,
    SnapshotterCompressionAlgorithm,
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
        let uri = FileUploader::upload(self, filepath).await?.into();

        Ok(AncillaryLocation::CloudStorage { uri })
    }
}

/// The [AncillaryArtifactBuilder] creates an ancillary archive from the cardano database directory (including ledger and volatile directories).
/// The archive is uploaded with the provided uploaders.
pub struct AncillaryArtifactBuilder {
    db_dir: PathBuf,
    uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
    logger: Logger,
}

impl AncillaryArtifactBuilder {
    #[deprecated]
    pub fn new(uploaders: Vec<Arc<dyn AncillaryFileUploader>>) -> Self {
        Self {
            db_dir: PathBuf::new(),
            uploaders,
            logger: Logger::root(slog::Discard, slog::o!()),
        }
    }

    pub fn new_with_db_dir(
        db_dir: PathBuf,
        uploaders: Vec<Arc<dyn AncillaryFileUploader>>,
        logger: Logger,
    ) -> Self {
        Self {
            db_dir,
            uploaders,
            logger,
        }
    }

    fn create_archive(&self, immutable_file_number: ImmutableFileNumber) -> PathBuf {
        let tmp_dir = self.db_dir.clone();
        let parent_dir = tmp_dir.parent().unwrap().join("snapshot_dest");
        let snapshotter = CompressedArchiveSnapshotter::new(
            self.db_dir.clone(),
            parent_dir, // TODO change it
            SnapshotterCompressionAlgorithm::Gzip,
            self.logger.clone(), // TODO TestLogger should not be here
        )
        .unwrap();

        let snapshot = snapshotter
            .snapshot("ancillary.tar.gz") // TODO remove unwrap
            .expect("create_archive should not fail");

        snapshot.get_file_path().to_path_buf()
    }

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

    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn upload_archive_should_return_empty_locations_with_no_uploader() {
        let builder = AncillaryArtifactBuilder::new(vec![]);

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

        let builder = AncillaryArtifactBuilder::new(uploaders);

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

    fn unpack_archive(archive_file: &Path, dst: &Path) {
        let mut archive = Archive::new(File::open(archive_file).unwrap());
        archive.unpack(dst).unwrap();
    }

    #[tokio::test]
    async fn create_archive_should_embed_ledger_volatile_directories_and_last_immutables() {
        let test_dir = "cardano_database/create_archive";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2])
            .with_ledger_files(&["blocks-0.dat", "blocks-1.dat", "blocks-2.dat"])
            .with_volatile_files(&["437", "537", "637", "737"])
            .build();
        std::fs::create_dir(cardano_db.get_dir().join("whatever")).unwrap();

        let builder = AncillaryArtifactBuilder::new_with_db_dir(
            cardano_db.get_dir().to_path_buf(),
            vec![],
            TestLogger::stdout(),
        );

        let archive_file_path = builder.create_archive(1);

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
        let immutables_nb = std::fs::read_dir(dst.join("immutable")).unwrap().count();
        assert_eq!(3, immutables_nb);

        assert!(dst.join(LEDGER_DIR).exists());
        assert!(dst.join(VOLATILE_DIR).exists());
        assert!(!dst.join("whatever").exists());
    }

    // test to verify that the immutable file number archived is the immutable file number in 'create_archive' + 1
}
