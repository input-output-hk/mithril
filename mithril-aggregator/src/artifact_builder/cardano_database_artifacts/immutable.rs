use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use regex::Regex;
use slog::{error, Logger};

use mithril_common::{
    digesters::{immutable_trio_names, IMMUTABLE_DIR},
    entities::{CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation, MultiFilesUri},
    logging::LoggerExtensions,
    StdResult,
};

use crate::{
    file_uploaders::{GcpUploader, LocalUploader},
    services::Snapshotter,
    tools::file_size,
    DumbUploader, FileUploader,
};

fn immmutable_file_number_extractor(file_uri: &str) -> StdResult<Option<String>> {
    let regex = Regex::new(r".*(\d{5})")?;

    Ok(regex
        .captures(file_uri)
        .and_then(|mat| mat.get(1))
        .map(|immutable_match| {
            let mut template = file_uri.to_string();
            template.replace_range(immutable_match.range(), "{immutable_file_number}");

            template
        }))
}

/// The [ImmutableFilesUploader] trait allows identifying uploaders that return locations for immutable files archive.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ImmutableFilesUploader: Send + Sync {
    /// Uploads the archives at the given filepaths and returns the location of the uploaded file.
    async fn batch_upload(
        &self,
        filepaths: &[PathBuf],
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<ImmutablesLocation>;
}

#[async_trait]
impl ImmutableFilesUploader for DumbUploader {
    async fn batch_upload(
        &self,
        filepaths: &[PathBuf],
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<ImmutablesLocation> {
        let last_file_path = filepaths.last().ok_or_else(|| {
            anyhow!("No file to upload with 'DumbUploader' as the filepaths list is empty")
        })?;

        let template_uri = MultiFilesUri::extract_template_from_uris(
            vec![self.upload(last_file_path).await?.into()],
            immmutable_file_number_extractor,
        )?
        .ok_or_else(|| {
            anyhow!("No matching template found in the uploaded files with 'DumbUploader'")
        })?;

        Ok(ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(template_uri),
            compression_algorithm,
        })
    }
}

#[async_trait]
impl ImmutableFilesUploader for LocalUploader {
    async fn batch_upload(
        &self,
        filepaths: &[PathBuf],
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<ImmutablesLocation> {
        let mut file_uris = Vec::new();
        for filepath in filepaths {
            file_uris.push(self.upload(filepath).await?.into());
        }

        let template_uri =
            MultiFilesUri::extract_template_from_uris(file_uris, immmutable_file_number_extractor)?
                .ok_or_else(|| {
                    anyhow!("No matching template found in the uploaded files with 'LocalUploader'")
                })?;

        Ok(ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(template_uri),
            compression_algorithm,
        })
    }
}

#[async_trait]
impl ImmutableFilesUploader for GcpUploader {
    async fn batch_upload(
        &self,
        filepaths: &[PathBuf],
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> StdResult<ImmutablesLocation> {
        let mut file_uris = Vec::new();
        for filepath in filepaths {
            file_uris.push(self.upload(filepath).await?.into());
        }

        let template_uri =
            MultiFilesUri::extract_template_from_uris(file_uris, immmutable_file_number_extractor)?
                .ok_or_else(|| {
                    anyhow!("No matching template found in the uploaded files with 'GcpUploader'")
                })?;

        Ok(ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(template_uri),
            compression_algorithm,
        })
    }
}

pub struct ImmutableArtifactBuilder {
    immutables_storage_dir: PathBuf,
    uploaders: Vec<Arc<dyn ImmutableFilesUploader>>,
    snapshotter: Arc<dyn Snapshotter>,
    logger: Logger,
}

impl ImmutableArtifactBuilder {
    pub fn new(
        immutables_storage_dir: PathBuf,
        uploaders: Vec<Arc<dyn ImmutableFilesUploader>>,
        snapshotter: Arc<dyn Snapshotter>,
        logger: Logger,
    ) -> StdResult<Self> {
        if uploaders.is_empty() {
            return Err(anyhow!(
                "At least one uploader is required to create an 'ImmutableArtifactBuilder'"
            ));
        }

        if !immutables_storage_dir.exists() {
            fs::create_dir(&immutables_storage_dir).with_context(|| {
                format!(
                    "Can not create immutable storage directory: '{}'",
                    immutables_storage_dir.display()
                )
            })?;
        }

        Ok(Self {
            immutables_storage_dir,
            uploaders,
            snapshotter,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    pub async fn upload(
        &self,
        up_to_immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let (archives_paths, compression_algorithm) =
            self.immutable_archives_paths_creating_the_missing_ones(up_to_immutable_file_number)?;
        let locations = self
            .upload_immutable_archives(&archives_paths, compression_algorithm)
            .await?;

        Ok(locations)
    }

    pub fn immutable_archives_paths_creating_the_missing_ones(
        &self,
        up_to_immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<(Vec<PathBuf>, CompressionAlgorithm)> {
        let mut archive_paths = vec![];
        let compression_algorithm = self.snapshotter.compression_algorithm();

        for immutable_file_number in 1..=up_to_immutable_file_number {
            let archive_name_without_extension = format!("{immutable_file_number:05}");
            let archive_name = format!(
                "{archive_name_without_extension}.{}",
                compression_algorithm.tar_file_extension()
            );

            if let Some(existing_archive) = self.retrieve_existing_snapshot_archive(&archive_name) {
                archive_paths.push(existing_archive);
            } else {
                let snapshot = self.snapshotter.snapshot_immutable_trio(
                    immutable_file_number,
                    &archive_name_without_extension,
                )?;

                let target_path = self.immutables_storage_dir.join(&archive_name);
                fs::rename(snapshot.get_file_path(), &target_path).with_context(|| {
                    format!(
                        "Can not move archive of immutable {immutable_file_number} from '{}' to '{}'",
                        snapshot.get_file_path().display(),
                        target_path.display()
                    )
                })?;

                archive_paths.push(target_path);
            }
        }

        Ok((archive_paths, compression_algorithm))
    }

    async fn upload_immutable_archives(
        &self,
        archive_paths: &[PathBuf],
        compression_algorithm: CompressionAlgorithm,
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            let result = uploader
                .batch_upload(archive_paths, Some(compression_algorithm))
                .await;
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

    fn retrieve_existing_snapshot_archive(&self, expected_archive_name: &str) -> Option<PathBuf> {
        let expected_archive_path = self.immutables_storage_dir.join(expected_archive_name);
        expected_archive_path
            .exists()
            .then_some(expected_archive_path)
    }

    pub fn compute_average_uncompressed_size(
        &self,
        db_path: &Path,
        up_to_immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<u64> {
        if up_to_immutable_file_number == 0 {
            return Err(anyhow!(
                "Could not compute the average size without immutable files"
            ));
        }

        let immutable_paths = (1..=up_to_immutable_file_number)
            .flat_map(immutable_trio_names)
            .map(|filename| db_path.join(IMMUTABLE_DIR).join(filename))
            .collect();

        let total_size = file_size::compute_size(immutable_paths)?;

        Ok(total_size / up_to_immutable_file_number)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        current_function,
        digesters::DummyCardanoDbBuilder,
        entities::TemplateUri,
        test_utils::{assert_equivalent, equivalent_to, TempDir},
    };
    use std::fs::File;
    use std::io::Write;

    use crate::services::{CompressedArchiveSnapshotter, DumbSnapshotter, MockSnapshotter};
    use crate::test_tools::TestLogger;
    use crate::tools::file_archiver::FileArchiver;

    use super::*;

    fn fake_uploader(
        archive_paths: Vec<&str>,
        location_uri: &str,
        compression_algorithm: Option<CompressionAlgorithm>,
    ) -> MockImmutableFilesUploader {
        let uri = location_uri.to_string();
        let archive_paths: Vec<_> = archive_paths.into_iter().map(String::from).collect();

        let mut uploader = MockImmutableFilesUploader::new();
        uploader
            .expect_batch_upload()
            .withf(move |p, algorithm| {
                let paths: Vec<_> = p.iter().map(|s| s.to_string_lossy().into_owned()).collect();

                equivalent_to(paths, archive_paths.clone()) && algorithm == &compression_algorithm
            })
            .times(1)
            .return_once(move |_, _| {
                Ok(ImmutablesLocation::CloudStorage {
                    uri: MultiFilesUri::Template(TemplateUri(uri)),
                    compression_algorithm,
                })
            });

        uploader
    }

    fn fake_uploader_returning_error() -> MockImmutableFilesUploader {
        let mut uploader = MockImmutableFilesUploader::new();
        uploader
            .expect_batch_upload()
            .return_once(|_, _| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn create_fake_file(path: &Path, content: &str) {
        let mut file = File::create(path).unwrap();
        write!(file, "{content}").unwrap();
    }

    macro_rules! assert_file_content {
        ($path:expr, $expected_content:expr) => {
            assert!($path.exists());
            let content = std::fs::read_to_string(&$path).unwrap();
            assert_eq!(content, $expected_content);
        };
    }

    fn get_builder_work_dir<N: Into<String>>(test_name: N) -> PathBuf {
        TempDir::create("cdb_immutable_builder", test_name)
    }

    #[tokio::test]
    async fn upload_call_archive_creation_and_upload_to_retrieve_locations() {
        let work_dir = get_builder_work_dir("upload_call_archive_creation_and_upload");
        let test_dir = "upload_call_archive_creation_and_upload/cardano_database";
        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2])
            .build();

        let db_directory = cardano_db.get_dir().to_path_buf();
        let snapshotter = CompressedArchiveSnapshotter::new(
            db_directory.clone(),
            db_directory.parent().unwrap().join("snapshot_dest"),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(work_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        let uploader = fake_uploader(
            vec![
                work_dir.join("00001.tar.gz").to_str().unwrap(),
                work_dir.join("00002.tar.gz").to_str().unwrap(),
            ],
            "archive.tar.gz",
            Some(CompressionAlgorithm::Gzip),
        );

        let builder = ImmutableArtifactBuilder::new(
            work_dir,
            vec![Arc::new(uploader)],
            Arc::new(snapshotter),
            TestLogger::stdout(),
        )
        .unwrap();

        let archive_paths = builder.upload(2).await.unwrap();

        assert_equivalent(
            archive_paths,
            vec![ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri("archive.tar.gz".to_string())),
                compression_algorithm: Some(CompressionAlgorithm::Gzip),
            }],
        )
    }

    #[test]
    fn create_immutable_builder_should_create_immutable_storage_dir_if_not_exist() {
        let work_dir = get_builder_work_dir(
            "create_immutable_builder_should_create_immutable_storage_dir_if_not_exist",
        );
        let immutable_storage_dir = work_dir.join("immutable");

        assert!(!immutable_storage_dir.exists());

        ImmutableArtifactBuilder::new(
            immutable_storage_dir.clone(),
            vec![Arc::new(DumbUploader::default())],
            Arc::new(DumbSnapshotter::default()),
            TestLogger::stdout(),
        )
        .unwrap();

        assert!(immutable_storage_dir.exists());
    }

    #[test]
    fn create_immutable_builder_should_not_create_or_remove_immutable_storage_dir_if_it_exist() {
        let immutable_storage_dir = get_builder_work_dir(
            "create_immutable_builder_should_not_create_or_remove_immutable_storage_dir_if_it_exist",
        );
        let existing_file_path = immutable_storage_dir.join("file.txt");
        create_fake_file(&existing_file_path, "existing file content");

        ImmutableArtifactBuilder::new(
            immutable_storage_dir,
            vec![Arc::new(DumbUploader::default())],
            Arc::new(DumbSnapshotter::default()),
            TestLogger::stdout(),
        )
        .unwrap();

        assert_file_content!(existing_file_path, "existing file content");
    }

    mod create_archive {
        use super::*;

        #[test]
        fn snapshot_immutables_files_up_to_the_given_immutable_file_number() {
            let work_dir = get_builder_work_dir(
                "snapshot_immutables_files_up_to_the_given_immutable_file_number",
            );
            let test_dir =
                "snapshot_immutables_files_up_to_the_given_immutable_file_number/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 2])
                .build();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                CompressionAlgorithm::Gzip,
                Arc::new(FileArchiver::new_for_test(work_dir.join("verification"))),
                TestLogger::stdout(),
            )
            .unwrap();

            let builder = ImmutableArtifactBuilder::new(
                work_dir.clone(),
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                TestLogger::stdout(),
            )
            .unwrap();

            let (archive_paths, _) = builder
                .immutable_archives_paths_creating_the_missing_ones(2)
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![work_dir.join("00001.tar.gz"), work_dir.join("00002.tar.gz")],
            )
        }

        #[test]
        fn return_error_when_one_of_the_three_immutable_files_is_missing() {
            let work_dir = get_builder_work_dir(
                "return_error_when_one_of_the_three_immutable_files_is_missing",
            );
            let test_dir =
                "error_when_one_of_the_three_immutable_files_is_missing/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 2])
                .build();

            let file_to_remove = cardano_db.get_immutable_dir().join("00002.chunk");
            std::fs::remove_file(file_to_remove).unwrap();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                CompressionAlgorithm::Gzip,
                Arc::new(FileArchiver::new_for_test(work_dir.join("verification"))),
                TestLogger::stdout(),
            )
            .unwrap();

            let builder = ImmutableArtifactBuilder::new(
                work_dir,
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                TestLogger::stdout(),
            )
            .unwrap();

            builder
                .immutable_archives_paths_creating_the_missing_ones(2)
                .expect_err(
                    "Should return an error when one of the three immutable files is missing",
                );
        }

        #[test]
        fn return_error_when_an_immutable_file_trio_is_missing() {
            let work_dir =
                get_builder_work_dir("return_error_when_an_immutable_file_trio_is_missing");
            let test_dir = "error_when_an_immutable_file_trio_is_missing/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 3])
                .build();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                CompressionAlgorithm::Gzip,
                Arc::new(FileArchiver::new_for_test(work_dir.join("verification"))),
                TestLogger::stdout(),
            )
            .unwrap();

            let builder = ImmutableArtifactBuilder::new(
                work_dir,
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                TestLogger::stdout(),
            )
            .unwrap();

            builder
                .immutable_archives_paths_creating_the_missing_ones(3)
                .expect_err("Should return an error when an immutable file trio is missing");
        }

        #[test]
        fn return_error_when_immutable_file_number_is_not_produced_yet() {
            let work_dir =
                get_builder_work_dir("return_error_when_immutable_file_number_is_not_produced_yet");
            let test_dir = "error_when_up_to_immutable_file_number_is_missing/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 2])
                .build();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                CompressionAlgorithm::Gzip,
                Arc::new(FileArchiver::new_for_test(work_dir.join("verification"))),
                TestLogger::stdout(),
            )
            .unwrap();

            let builder = ImmutableArtifactBuilder::new(
                work_dir,
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                TestLogger::stdout(),
            )
            .unwrap();

            builder
                .immutable_archives_paths_creating_the_missing_ones(3)
                .expect_err("Should return an error when an immutable file trio is missing");
        }

        #[test]
        fn test_retrieve_existing_snapshot_archive() {
            let work_dir = get_builder_work_dir("retrieve_existing_snapshot_archive(");
            let file_name = "whatever.txt";

            let builder = ImmutableArtifactBuilder::new(
                work_dir.clone(),
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(MockSnapshotter::new()),
                TestLogger::stdout(),
            )
            .unwrap();

            assert_eq!(builder.retrieve_existing_snapshot_archive(file_name), None);

            fs::File::create(work_dir.join(file_name)).unwrap();

            assert_eq!(
                builder.retrieve_existing_snapshot_archive(file_name),
                Some(work_dir.join(file_name))
            );
        }

        #[test]
        fn return_all_archives_but_not_rebuild_archives_already_compressed() {
            let work_dir = get_builder_work_dir("return_all_archives_but_not_rebuild_archives");
            let test_dir = "return_all_archives_but_not_rebuild_archives/cardano_database";
            let cardano_db = DummyCardanoDbBuilder::new(test_dir)
                .with_immutables(&[1, 2, 3])
                .build();

            let db_directory = cardano_db.get_dir().to_path_buf();
            let snapshotter = CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                db_directory.parent().unwrap().join("snapshot_dest"),
                CompressionAlgorithm::Gzip,
                Arc::new(FileArchiver::new_for_test(work_dir.join("verification"))),
                TestLogger::stdout(),
            )
            .unwrap();

            create_fake_file(&work_dir.join("00001.tar.gz"), "00001 content");
            create_fake_file(&work_dir.join("00002.tar.gz"), "00002 content");

            let builder = ImmutableArtifactBuilder::new(
                work_dir.clone(),
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                TestLogger::stdout(),
            )
            .unwrap();

            let (archive_paths, _) = builder
                .immutable_archives_paths_creating_the_missing_ones(3)
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    work_dir.join("00001.tar.gz"),
                    work_dir.join("00002.tar.gz"),
                    work_dir.join("00003.tar.gz"),
                ],
            );
            // Check that the existing archives content have not changed
            assert_file_content!(work_dir.join("00001.tar.gz"), "00001 content");
            assert_file_content!(work_dir.join("00002.tar.gz"), "00002 content");
        }

        #[test]
        fn return_all_archives_paths_even_if_all_archives_already_exist() {
            let work_dir =
                get_builder_work_dir("return_all_archives_paths_even_if_all_archives_exist");
            let mut snapshotter = MockSnapshotter::new();
            snapshotter
                .expect_compression_algorithm()
                .returning(|| CompressionAlgorithm::Gzip);
            snapshotter.expect_snapshot_subset().never();

            create_fake_file(&work_dir.join("00001.tar.gz"), "00001 content");
            create_fake_file(&work_dir.join("00002.tar.gz"), "00002 content");
            create_fake_file(&work_dir.join("00003.tar.gz"), "00003 content");

            let builder = ImmutableArtifactBuilder::new(
                work_dir.clone(),
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                TestLogger::stdout(),
            )
            .unwrap();

            let (archive_paths, _) = builder
                .immutable_archives_paths_creating_the_missing_ones(3)
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    work_dir.join("00001.tar.gz"),
                    work_dir.join("00002.tar.gz"),
                    work_dir.join("00003.tar.gz"),
                ],
            )
        }
    }

    mod upload {
        use mithril_common::test_utils::TempDir;

        use super::MockImmutableFilesUploader;

        use super::*;

        #[test]
        fn create_immutable_builder_should_error_when_no_uploader() {
            let result = ImmutableArtifactBuilder::new(
                get_builder_work_dir("create_immutable_builder_should_error_when_no_uploader"),
                vec![],
                Arc::new(DumbSnapshotter::default()),
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
                .expect_batch_upload()
                .return_once(|_, _| Err(anyhow!("Failure while uploading...")));

            {
                let builder = ImmutableArtifactBuilder::new(
                    get_builder_work_dir("upload_immutable_archives_should_log_upload_errors"),
                    vec![Arc::new(uploader)],
                    Arc::new(MockSnapshotter::new()),
                    TestLogger::file(&log_path),
                )
                .unwrap();

                let _ = builder
                    .upload_immutable_archives(
                        &[PathBuf::from("01.tar.gz"), PathBuf::from("02.tar.gz")],
                        CompressionAlgorithm::Gzip,
                    )
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
                get_builder_work_dir("upload_immutable_archives_should_error_when_no_location"),
                uploaders,
                Arc::new(MockSnapshotter::new()),
                TestLogger::stdout(),
            )
            .unwrap();

            let result = builder
                .upload_immutable_archives(
                    &[PathBuf::from("01.tar.gz"), PathBuf::from("02.tar.gz")],
                    CompressionAlgorithm::Gzip,
                )
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
                    Some(CompressionAlgorithm::Gzip),
                )),
                Arc::new(fake_uploader_returning_error()),
            ];

            let builder = ImmutableArtifactBuilder::new(
                get_builder_work_dir(
                    "upload_immutable_archives_should_return_location_even_with_uploaders_errors",
                ),
                uploaders,
                Arc::new(MockSnapshotter::new()),
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .upload_immutable_archives(
                    &[PathBuf::from("01.tar.gz"), PathBuf::from("02.tar.gz")],
                    CompressionAlgorithm::Gzip,
                )
                .await
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![ImmutablesLocation::CloudStorage {
                    uri: MultiFilesUri::Template(TemplateUri("archive_2.tar.gz".to_string())),
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                }],
            )
        }

        #[tokio::test]
        async fn upload_immutable_archives_should_return_all_uploaders_returned_locations() {
            let uploaders: Vec<Arc<dyn ImmutableFilesUploader>> = vec![
                Arc::new(fake_uploader(
                    vec!["01.tar.gz", "02.tar.gz"],
                    "archive_1.tar.gz",
                    Some(CompressionAlgorithm::Gzip),
                )),
                Arc::new(fake_uploader(
                    vec!["01.tar.gz", "02.tar.gz"],
                    "archive_2.tar.gz",
                    Some(CompressionAlgorithm::Gzip),
                )),
            ];

            let builder = ImmutableArtifactBuilder::new(
                get_builder_work_dir(
                    "upload_immutable_archives_should_return_all_uploaders_returned_locations",
                ),
                uploaders,
                Arc::new(MockSnapshotter::new()),
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .upload_immutable_archives(
                    &[PathBuf::from("01.tar.gz"), PathBuf::from("02.tar.gz")],
                    CompressionAlgorithm::Gzip,
                )
                .await
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri("archive_1.tar.gz".to_string())),
                        compression_algorithm: Some(CompressionAlgorithm::Gzip),
                    },
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri("archive_2.tar.gz".to_string())),
                        compression_algorithm: Some(CompressionAlgorithm::Gzip),
                    },
                ],
            )
        }
    }

    mod batch_upload {
        use crate::file_uploaders::FileUploadRetryPolicy;
        use crate::tools::url_sanitizer::SanitizedUrlWithTrailingSlash;
        use mithril_common::test_utils::TempDir;

        use super::*;

        fn create_fake_archive(dir: &Path, name: &str) -> PathBuf {
            let file_path = dir.join(name);
            create_fake_file(
                &file_path,
                "I swear, this is an archive, not a temporary test file.",
            );
            file_path
        }

        #[tokio::test]
        async fn extract_archive_name_to_deduce_template_location() {
            let source_dir = TempDir::create(
                "immutable",
                "extract_archive_name_to_deduce_template_location_source",
            );
            let target_dir = TempDir::create(
                "immutable",
                "extract_archive_name_to_deduce_template_location_target",
            );

            let archive_1 = create_fake_archive(&source_dir, "00001.tar.gz");
            let archive_2 = create_fake_archive(&source_dir, "00002.tar.gz");

            let url_prefix =
                SanitizedUrlWithTrailingSlash::parse("http://test.com:8080/base-root").unwrap();
            let uploader = LocalUploader::new(
                url_prefix,
                &target_dir,
                FileUploadRetryPolicy::never(),
                TestLogger::stdout(),
            );
            let location = ImmutableFilesUploader::batch_upload(
                &uploader,
                &[archive_1.clone(), archive_2.clone()],
                None,
            )
            .await
            .expect("local upload should not fail");

            assert!(target_dir.join(archive_1.file_name().unwrap()).exists());
            assert!(target_dir.join(archive_2.file_name().unwrap()).exists());

            let expected_location = ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri(
                    "http://test.com:8080/base-root/{immutable_file_number}.tar.gz".to_string(),
                )),
                compression_algorithm: None,
            };
            assert_eq!(expected_location, location);
        }

        #[tokio::test]
        async fn returns_error_when_uploaded_filename_not_templatable_without_5_digits() {
            let source_dir = TempDir::create(
                "immutable",
                "returns_error_when_uploaded_filename_not_templatable",
            );
            let target_dir = TempDir::create(
                "immutable",
                "returns_error_when_uploaded_filename_not_templatable",
            );

            let archive = create_fake_archive(&source_dir, "not-templatable.tar.gz");

            let url_prefix =
                SanitizedUrlWithTrailingSlash::parse("http://test.com:8080/base-root").unwrap();
            let uploader = LocalUploader::new(
                url_prefix,
                &target_dir,
                FileUploadRetryPolicy::never(),
                TestLogger::stdout(),
            );

            ImmutableFilesUploader::batch_upload(&uploader, &[archive], None)
                .await
                .expect_err("Should return an error when not template found");
        }
    }

    mod immutable_file_number_extractor {
        use super::*;

        #[test]
        fn returns_none_when_not_templatable_without_5_digits() {
            let template = immmutable_file_number_extractor("not-templatable.tar.gz").unwrap();

            assert!(template.is_none());
        }

        #[test]
        fn returns_template() {
            let template =
                immmutable_file_number_extractor("http://whatever/00001.tar.gz").unwrap();

            assert_eq!(
                template,
                Some("http://whatever/{immutable_file_number}.tar.gz".to_string())
            );
        }

        #[test]
        fn replaces_last_occurence_of_5_digits() {
            let template =
                immmutable_file_number_extractor("http://00001/whatever/00001.tar.gz").unwrap();

            assert_eq!(
                template,
                Some("http://00001/whatever/{immutable_file_number}.tar.gz".to_string())
            );
        }

        #[test]
        fn replaces_last_occurence_when_more_than_5_digits() {
            let template =
                immmutable_file_number_extractor("http://whatever/123456789.tar.gz").unwrap();

            assert_eq!(
                template,
                Some("http://whatever/1234{immutable_file_number}.tar.gz".to_string())
            );
        }
    }

    #[test]
    fn should_compute_the_average_size_of_the_immutables() {
        let work_dir = get_builder_work_dir("should_compute_the_average_size_of_the_immutables");
        let test_dir = "should_compute_the_average_size_of_the_immutables/cardano_database";

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

        let builder = ImmutableArtifactBuilder::new(
            work_dir,
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(DumbSnapshotter::default()),
            TestLogger::stdout(),
        )
        .unwrap();

        let expected_total_size = immutable_trio_file_size;

        let total_size = builder
            .compute_average_uncompressed_size(&db_directory, 2)
            .unwrap();

        assert_eq!(expected_total_size, total_size);
    }

    #[test]
    fn should_return_an_error_when_compute_the_average_size_of_the_immutables_with_zero() {
        let work_dir = get_builder_work_dir("should_compute_the_average_size_of_the_immutables");
        let test_dir = &format!("{}/cardano_database", current_function!());

        let cardano_db = DummyCardanoDbBuilder::new(test_dir).build();

        let db_directory = cardano_db.get_dir().to_path_buf();

        let builder = ImmutableArtifactBuilder::new(
            work_dir,
            vec![Arc::new(MockImmutableFilesUploader::new())],
            Arc::new(DumbSnapshotter::default()),
            TestLogger::stdout(),
        )
        .unwrap();

        builder
            .compute_average_uncompressed_size(&db_directory, 0)
            .expect_err("Should return an error when no immutable file number");
    }
}
