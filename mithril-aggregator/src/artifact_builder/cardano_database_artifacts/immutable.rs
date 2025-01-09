use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::anyhow;
use async_trait::async_trait;

use mithril_common::{
    digesters::IMMUTABLE_DIR,
    entities::{CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation},
    logging::LoggerExtensions,
    StdResult,
};
use slog::{error, Logger};

use crate::Snapshotter;

/// The [ImmutableFilesUploader] trait allows identifying uploaders that return locations for immutable files archive.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ImmutableFilesUploader: Send + Sync {
    /// Uploads the archives at the given filepaths and returns the location of the uploaded file.
    async fn upload<'a>(&self, filepaths: &[&'a Path]) -> StdResult<ImmutablesLocation>;
}

pub struct ImmutableArtifactBuilder {
    uploaders: Vec<Arc<dyn ImmutableFilesUploader>>,
    snapshotter: Arc<dyn Snapshotter>,
    compression_algorithm: CompressionAlgorithm,
    logger: Logger,
}

impl ImmutableArtifactBuilder {
    pub fn new(
        uploaders: Vec<Arc<dyn ImmutableFilesUploader>>,
        snapshotter: Arc<dyn Snapshotter>,
        compression_algorithm: CompressionAlgorithm,
        logger: Logger,
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
            logger: logger.new_with_component_name::<Self>(),
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

    async fn upload_immutable_archives(
        &self,
        archive_paths: &[&Path],
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            let result = uploader.upload(archive_paths).await;
            match result {
                Ok(location) => {
                    locations.push(location);
                }
                Err(e) => {
                    error!(
                        self.logger,
                        "Failed to upload immutable archive";
                        "error" => e.to_string()
                    );
                }
            }
        }

        if locations.is_empty() {
            return Err(anyhow!(
                "Failed to upload immutable archive with all uploaders"
            ));
        }

        Ok(locations)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{digesters::DummyCardanoDbBuilder, test_utils::assert_equivalent};
    use mockall::predicate::eq;
    use uuid::Uuid;

    use crate::{
        snapshotter::{MockSnapshotter, OngoingSnapshot},
        test_tools::TestLogger,
        CompressedArchiveSnapshotter, DumbSnapshotter, SnapshotterCompressionAlgorithm,
    };

    use super::*;

    #[test]
    fn upload_should_return_locations() {
        let test_dir = "upload_should_return_locations/cardano_database";
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
            TestLogger::stdout(),
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

    mod create_archive {

        use super::*;

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
                TestLogger::stdout(),
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
            let test_dir =
                "error_when_one_of_the_three_immutable_files_is_missing/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 2])
                .build();

            let file_to_remove = cardano_db.get_immutable_dir().join("00002.chunk");
            std::fs::remove_file(file_to_remove).unwrap();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let mut snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();
            snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

            let builder = ImmutableArtifactBuilder::new(
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            builder.create_immutables_archives(2).expect_err(
                "Should return an error when one of the three immutable files is missing",
            );
        }

        #[test]
        fn create_immutables_archives_should_return_error_when_an_immutable_file_trio_is_missing() {
            let test_dir = "error_when_an_immutable_file_trio_is_missing/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 3])
                .build();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let mut snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();
            snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

            let builder = ImmutableArtifactBuilder::new(
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            builder
                .create_immutables_archives(3)
                .expect_err("Should return an error when an immutable file trio is missing");
        }

        #[test]
        fn create_immutables_archives_should_return_error_when_up_to_immutable_file_number_is_missing(
        ) {
            let test_dir = "error_when_up_to_immutable_file_number_is_missing/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 2])
                .build();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let mut snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();
            snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

            let builder = ImmutableArtifactBuilder::new(
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
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
                TestLogger::stdout(),
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

    mod upload {
        use mithril_common::test_utils::{equivalent_to, TempDir};

        use super::MockImmutableFilesUploader;

        use super::*;

        fn fake_uploader(
            archive_paths: Vec<&str>,
            location_uri: &str,
        ) -> MockImmutableFilesUploader {
            let uri = location_uri.to_string();
            let archive_paths: Vec<_> = archive_paths.into_iter().map(String::from).collect();

            let mut uploader = MockImmutableFilesUploader::new();
            uploader
                .expect_upload()
                .withf(move |p| {
                    let paths: Vec<_> =
                        p.iter().map(|s| s.to_string_lossy().into_owned()).collect();

                    equivalent_to(paths, archive_paths.clone())
                })
                .times(1)
                .return_once(|_| Ok(ImmutablesLocation::CloudStorage { uri }));

            uploader
        }

        fn fake_uploader_returning_error() -> MockImmutableFilesUploader {
            let mut uploader = MockImmutableFilesUploader::new();
            uploader
                .expect_upload()
                .return_once(|_| Err(anyhow!("Failure while uploading...")));

            uploader
        }

        #[test]
        fn create_immutable_builder_should_error_when_no_uploader() {
            let result = ImmutableArtifactBuilder::new(
                vec![],
                Arc::new(DumbSnapshotter::new()),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            );

            assert!(result.is_err(), "Should return an error when no uploaders")
        }

        #[tokio::test]
        async fn upload_immutable_archives_should_log_upload_errors() {
            let log_path = TempDir::create(
                "immutable",
                "upload_immutable_archives_should_log_upload_errors",
            )
            .join("test.log");

            let mut uploader = MockImmutableFilesUploader::new();
            uploader
                .expect_upload()
                .return_once(|_| Err(anyhow!("Failure while uploading...")));

            {
                let builder = ImmutableArtifactBuilder::new(
                    vec![Arc::new(uploader)],
                    Arc::new(MockSnapshotter::new()),
                    CompressionAlgorithm::Gzip,
                    TestLogger::file(&log_path),
                )
                .unwrap();

                let _ = builder
                    .upload_immutable_archives(&vec![
                        Path::new("01.tar.gz"),
                        Path::new("02.tar.gz"),
                    ])
                    .await;
            }

            let logs = std::fs::read_to_string(&log_path).unwrap();
            assert!(logs.contains("Failure while uploading..."));
        }

        #[tokio::test]
        async fn upload_immutable_archives_should_error_when_no_location_is_returned() {
            let uploaders: Vec<Arc<dyn ImmutableFilesUploader>> =
                vec![Arc::new(fake_uploader_returning_error())];

            let builder = ImmutableArtifactBuilder::new(
                uploaders,
                Arc::new(MockSnapshotter::new()),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            let result = builder
                .upload_immutable_archives(&vec![Path::new("01.tar.gz"), Path::new("02.tar.gz")])
                .await;

            assert!(
                result.is_err(),
                "Should return an error when no location is returned"
            );
        }

        #[tokio::test]
        async fn upload_immutable_archives_should_return_location_even_with_uploaders_errors() {
            let uploaders: Vec<Arc<dyn ImmutableFilesUploader>> = vec![
                Arc::new(fake_uploader_returning_error()),
                Arc::new(fake_uploader(
                    vec!["01.tar.gz", "02.tar.gz"],
                    "archive_2.tar.gz",
                )),
                Arc::new(fake_uploader_returning_error()),
            ];

            let builder = ImmutableArtifactBuilder::new(
                uploaders,
                Arc::new(MockSnapshotter::new()),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .upload_immutable_archives(&vec![Path::new("01.tar.gz"), Path::new("02.tar.gz")])
                .await
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![ImmutablesLocation::CloudStorage {
                    uri: "archive_2.tar.gz".to_string(),
                }],
            )
        }

        #[tokio::test]
        async fn upload_immutable_archives_should_return_all_uploaders_returned_locations() {
            let uploaders: Vec<Arc<dyn ImmutableFilesUploader>> = vec![
                Arc::new(fake_uploader(
                    vec!["01.tar.gz", "02.tar.gz"],
                    "archive_1.tar.gz",
                )),
                Arc::new(fake_uploader(
                    vec!["01.tar.gz", "02.tar.gz"],
                    "archive_2.tar.gz",
                )),
            ];

            let builder = ImmutableArtifactBuilder::new(
                uploaders,
                Arc::new(MockSnapshotter::new()),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .upload_immutable_archives(&vec![Path::new("01.tar.gz"), Path::new("02.tar.gz")])
                .await
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    ImmutablesLocation::CloudStorage {
                        uri: "archive_1.tar.gz".to_string(),
                    },
                    ImmutablesLocation::CloudStorage {
                        uri: "archive_2.tar.gz".to_string(),
                    },
                ],
            )
        }
    }
}
