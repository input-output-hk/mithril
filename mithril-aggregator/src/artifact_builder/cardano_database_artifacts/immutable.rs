use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::anyhow;
use async_trait::async_trait;

use mithril_common::{
    digesters::IMMUTABLE_DIR,
    entities::{CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation},
    StdResult,
};

use crate::Snapshotter;

/// The [ImmutableFilesUploader] trait allows identifying uploaders that return locations for immutable files archive.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ImmutableFilesUploader: Send + Sync {
    /// Uploads the archives at the given filepaths and returns the location of the uploaded file.
    async fn upload<'a>(&self, filepaths: Vec<&'a Path>) -> StdResult<ImmutablesLocation>;
}

pub struct ImmutableArtifactBuilder {
    uploaders: Vec<Arc<dyn ImmutableFilesUploader>>,
    snapshotter: Arc<dyn Snapshotter>,
    compression_algorithm: CompressionAlgorithm,
}

impl ImmutableArtifactBuilder {
    pub fn new(
        uploaders: Vec<Arc<dyn ImmutableFilesUploader>>,
        snapshotter: Arc<dyn Snapshotter>,
        compression_algorithm: CompressionAlgorithm,
    ) -> StdResult<Self> {
        if uploaders.is_empty() {
            return Err(anyhow!(
                "At least one uploader is required to create an 'ImmutableArtifactBuilder'"
            ));
        }

        Ok(Self {
            uploaders,
            snapshotter,
            compression_algorithm,
        })
    }

    pub fn upload(
        &self,
        up_to_immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let _archives_paths = self.create_immutables_archives(up_to_immutable_file_number)?;

        Ok(vec![])
    }

    pub fn create_immutables_archives(
        &self,
        up_to_immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Vec<PathBuf>> {
        fn immutable_trio_names(immutable_file_number: ImmutableFileNumber) -> Vec<String> {
            vec![
                format!("{:05}.chunk", immutable_file_number),
                format!("{:05}.primary", immutable_file_number),
                format!("{:05}.secondary", immutable_file_number),
            ]
        }

        let immutable_archive_path = Path::new("cardano-database").join("immutable");

        let mut archive_paths = vec![];
        for immutable_file_number in 1..=up_to_immutable_file_number {
            let files_to_archive = immutable_trio_names(immutable_file_number)
                .iter()
                .map(|filename| PathBuf::from(IMMUTABLE_DIR).join(filename))
                .collect();

            let archive_name = format!(
                "{:05}.{}",
                immutable_file_number,
                self.compression_algorithm.tar_file_extension()
            );

            if !self
                .snapshotter
                .is_snapshot_exist(&immutable_archive_path.join(archive_name.clone()))
            {
                let archive_path = self.snapshotter.snapshot_subset(
                    &immutable_archive_path.join(archive_name),
                    files_to_archive,
                )?;
                archive_paths.push(archive_path.get_file_path().to_path_buf());
            }
        }

        Ok(archive_paths)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{digesters::DummyCardanoDbBuilder, test_utils::assert_equivalent};
    use mockall::predicate::eq;

    use crate::{
        file_uploaders::MockFileUploader,
        snapshotter::{MockSnapshotter, OngoingSnapshot},
        test_tools::TestLogger,
        CompressedArchiveSnapshotter, DumbSnapshotter, SnapshotterCompressionAlgorithm,
    };

    use super::*;

    #[test]
    fn create_immutable_builder_should_error_when_no_uploader() {
        let result = ImmutableArtifactBuilder::new(
            vec![],
            Arc::new(DumbSnapshotter::new()),
            CompressionAlgorithm::Gzip,
        );

        assert!(result.is_err(), "Should return an error when no uploaders")
    }

    #[test]
    fn upload_should_return_locations() {
        let test_dir = "cardano_database/upload_should_return_locations";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2, 3])
            .build();

        let db_directory = cardano_db.get_dir().to_path_buf();
        let snapshot_directory = db_directory.parent().unwrap().join("snapshot_dest");
        let mut snapshotter = MockSnapshotter::new();
        snapshotter.expect_is_snapshot_exist().returning(|_| false);

        let dest_filepath = Path::new("/tmp");
        snapshotter
            .expect_snapshot_subset()
            .times(1)
            .with(
                eq(Path::new("cardano-database/immutable/00001.tar.gz")),
                eq(vec![
                    PathBuf::from(IMMUTABLE_DIR).join("00001.chunk"),
                    PathBuf::from(IMMUTABLE_DIR).join("00001.primary"),
                    PathBuf::from(IMMUTABLE_DIR).join("00001.secondary"),
                ]),
            )
            .returning(|filepath, _| Ok(OngoingSnapshot::new(dest_filepath.join(filepath), 0)));

        snapshotter
            .expect_snapshot_subset()
            .times(1)
            .with(
                eq(Path::new("cardano-database/immutable/00002.tar.gz")),
                eq(vec![
                    PathBuf::from(IMMUTABLE_DIR).join("00002.chunk"),
                    PathBuf::from(IMMUTABLE_DIR).join("00002.primary"),
                    PathBuf::from(IMMUTABLE_DIR).join("00002.secondary"),
                ]),
            )
            .returning(|filepath, _| Ok(OngoingSnapshot::new(dest_filepath.join(filepath), 0)));
        // let snapshotter = SnapshotterWrapper {
        //     snapshotter: {
        //         CompressedArchiveSnapshotter::new(
        //             db_directory.clone(),
        //             snapshot_directory.clone(),
        //             SnapshotterCompressionAlgorithm::Gzip,
        //             TestLogger::stdout(),
        //         )
        //         .unwrap()
        //     },
        // };

        let mut uploader = MockImmutableFilesUploader::new();
        uploader
            .expect_upload()
            .return_once(|_| Err(anyhow!("Failure while uploading, no location returned")));

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
        )
        .unwrap();

        builder.upload(2).unwrap();

        // let expected_archives_directory = snapshot_directory
        //     .join("cardano-database")
        //     .join("immutable");
        // assert!(expected_archives_directory.join("00001.tar.gz").exists());
        // assert!(expected_archives_directory.join("00002.tar.gz").exists());
        // let archives_nb = std::fs::read_dir(expected_archives_directory)
        //     .unwrap()
        //     .count();
        // assert_eq!(2, archives_nb);
    }

    #[test]
    fn create_immutables_archives_should_snapshot_immutables_files_up_to_the_given_immutable_file_number(
    ) {
        let mut snapshotter = MockSnapshotter::new();
        snapshotter.expect_is_snapshot_exist().returning(|_| false);

        snapshotter
            .expect_snapshot_subset()
            .times(1)
            .with(
                eq(Path::new("cardano-database/immutable/00001.tar.gz")),
                eq(vec![
                    PathBuf::from(IMMUTABLE_DIR).join("00001.chunk"),
                    PathBuf::from(IMMUTABLE_DIR).join("00001.primary"),
                    PathBuf::from(IMMUTABLE_DIR).join("00001.secondary"),
                ]),
            )
            .returning(|filepath, _| {
                let path = Path::new("/tmp").join(filepath);
                Ok(OngoingSnapshot::new(path, 0))
            });

        snapshotter
            .expect_snapshot_subset()
            .times(1)
            .with(
                eq(Path::new("cardano-database/immutable/00002.tar.gz")),
                eq(vec![
                    PathBuf::from(IMMUTABLE_DIR).join("00002.chunk"),
                    PathBuf::from(IMMUTABLE_DIR).join("00002.primary"),
                    PathBuf::from(IMMUTABLE_DIR).join("00002.secondary"),
                ]),
            )
            .returning(|filepath, _| {
                let path = Path::new("/tmp").join(filepath);
                Ok(OngoingSnapshot::new(path, 0))
            });

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
        )
        .unwrap();

        let archive_paths = builder.create_immutables_archives(2).unwrap();

        assert_equivalent(
            archive_paths,
            vec![
                PathBuf::from("/tmp/cardano-database/immutable/00001.tar.gz"),
                PathBuf::from("/tmp/cardano-database/immutable/00002.tar.gz"),
            ],
        )
    }

    #[test]
    fn create_immutables_archives_should_return_error_when_one_of_the_three_immutable_files_is_missing(
    ) {
        let test_dir = "cardano_database/error_when_one_of_the_three_immutable_files_is_missing";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2])
            .build();

        let file_to_remove = cardano_db.get_immutable_dir().join("00002.chunk");
        std::fs::remove_file(file_to_remove).unwrap();

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

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
        )
        .unwrap();

        builder
            .create_immutables_archives(2)
            .expect_err("Should return an error when one of the three immutable files is missing");
    }

    #[test]
    fn create_immutables_archives_should_return_error_when_an_immutable_file_trio_is_missing() {
        let test_dir = "cardano_database/error_when_an_immutable_file_trio_is_missing";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 3])
            .build();

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

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
        )
        .unwrap();

        builder
            .create_immutables_archives(3)
            .expect_err("Should return an error when an immutable file trio is missing");
    }

    #[test]
    fn create_immutables_archives_should_return_error_when_up_to_immutable_file_number_is_missing()
    {
        let test_dir = "cardano_database/error_when_up_to_immutable_file_number_is_missing";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2])
            .build();

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

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
        )
        .unwrap();

        builder
            .create_immutables_archives(3)
            .expect_err("Should return an error when an immutable file trio is missing");
    }

    #[test]
    fn create_immutables_archives_should_not_rebuild_archives_already_compressed() {
        let mut snapshotter = MockSnapshotter::new();

        snapshotter
            .expect_is_snapshot_exist()
            .times(1)
            .with(eq(Path::new("cardano-database/immutable/00001.tar.gz")))
            .returning(|_| true);
        snapshotter
            .expect_is_snapshot_exist()
            .times(1)
            .with(eq(Path::new("cardano-database/immutable/00002.tar.gz")))
            .returning(|_| true);
        snapshotter
            .expect_is_snapshot_exist()
            .times(1)
            .with(eq(Path::new("cardano-database/immutable/00003.tar.gz")))
            .returning(|_| false);
        snapshotter
            .expect_snapshot_subset()
            .times(1)
            .with(
                eq(Path::new("cardano-database/immutable/00003.tar.gz")),
                eq(vec![
                    PathBuf::from(IMMUTABLE_DIR).join("00003.chunk"),
                    PathBuf::from(IMMUTABLE_DIR).join("00003.primary"),
                    PathBuf::from(IMMUTABLE_DIR).join("00003.secondary"),
                ]),
            )
            .returning(|filepath, _| {
                let path = Path::new("/tmp").join(filepath);
                Ok(OngoingSnapshot::new(path, 0))
            });

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
        )
        .unwrap();

        let archive_paths = builder.create_immutables_archives(3).unwrap();

        assert_equivalent(
            archive_paths,
            vec![PathBuf::from(
                "/tmp/cardano-database/immutable/00003.tar.gz",
            )],
        )
    }
}
