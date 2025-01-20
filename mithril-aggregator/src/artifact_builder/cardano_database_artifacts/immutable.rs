use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::anyhow;
use async_trait::async_trait;
use regex::Regex;
use slog::{error, Logger};

use mithril_common::{
    digesters::IMMUTABLE_DIR,
    entities::{CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation, MultiFilesUri},
    logging::LoggerExtensions,
    StdResult,
};

use crate::{
    file_uploaders::{GcpUploader, LocalUploader},
    DumbUploader, FileUploader, Snapshotter,
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
    async fn batch_upload(&self, filepaths: &[PathBuf]) -> StdResult<ImmutablesLocation>;
}

#[async_trait]
impl ImmutableFilesUploader for DumbUploader {
    async fn batch_upload(&self, filepaths: &[PathBuf]) -> StdResult<ImmutablesLocation> {
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
        })
    }
}

#[async_trait]
impl ImmutableFilesUploader for LocalUploader {
    async fn batch_upload(&self, filepaths: &[PathBuf]) -> StdResult<ImmutablesLocation> {
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
        })
    }
}

#[async_trait]
impl ImmutableFilesUploader for GcpUploader {
    async fn batch_upload(&self, filepaths: &[PathBuf]) -> StdResult<ImmutablesLocation> {
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
        })
    }
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

    pub async fn upload(
        &self,
        up_to_immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let archives_paths =
            self.immutable_archives_paths_creating_the_missing_ones(up_to_immutable_file_number)?;
        let locations = self.upload_immutable_archives(&archives_paths).await?;

        Ok(locations)
    }

    pub fn immutable_archives_paths_creating_the_missing_ones(
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

        let immutable_archive_dir_path = Path::new("cardano-database").join("immutable");

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

            let immutable_archive_file_path = immutable_archive_dir_path.join(archive_name);
            if !self
                .snapshotter
                .does_snapshot_exist(&immutable_archive_file_path)
            {
                self.snapshotter
                    .snapshot_subset(&immutable_archive_file_path, files_to_archive)?;
            }
            archive_paths.push(self.snapshotter.get_file_path(&immutable_archive_file_path));
        }

        Ok(archive_paths)
    }

    async fn upload_immutable_archives(
        &self,
        archive_paths: &[PathBuf],
    ) -> StdResult<Vec<ImmutablesLocation>> {
        let mut locations = Vec::new();
        for uploader in &self.uploaders {
            let result = uploader.batch_upload(archive_paths).await;
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
    use mithril_common::{
        digesters::DummyCardanoDbBuilder,
        entities::TemplateUri,
        test_utils::{assert_equivalent, equivalent_to},
    };
    use mockall::predicate::{always, eq};
    use uuid::Uuid;

    use crate::{
        snapshotter::{MockSnapshotter, OngoingSnapshot},
        test_tools::TestLogger,
        CompressedArchiveSnapshotter, DumbSnapshotter, SnapshotterCompressionAlgorithm,
    };

    use super::*;

    fn fake_uploader(archive_paths: Vec<&str>, location_uri: &str) -> MockImmutableFilesUploader {
        let uri = location_uri.to_string();
        let archive_paths: Vec<_> = archive_paths.into_iter().map(String::from).collect();

        let mut uploader = MockImmutableFilesUploader::new();
        uploader
            .expect_batch_upload()
            .withf(move |p| {
                let paths: Vec<_> = p.iter().map(|s| s.to_string_lossy().into_owned()).collect();

                equivalent_to(paths, archive_paths.clone())
            })
            .times(1)
            .return_once(|_| {
                Ok(ImmutablesLocation::CloudStorage {
                    uri: MultiFilesUri::Template(TemplateUri(uri)),
                })
            });

        uploader
    }

    fn fake_uploader_returning_error() -> MockImmutableFilesUploader {
        let mut uploader = MockImmutableFilesUploader::new();
        uploader
            .expect_batch_upload()
            .return_once(|_| Err(anyhow!("Failure while uploading...")));

        uploader
    }

    fn dummy_ongoing_snapshot() -> OngoingSnapshot {
        OngoingSnapshot::new(PathBuf::from("/whatever"), 0)
    }

    #[tokio::test]
    async fn upload_call_archive_creation_and_upload_to_retrieve_locations() {
        let snapshotter = {
            let mut snapshotter = MockSnapshotter::new();
            snapshotter
                .expect_does_snapshot_exist()
                .returning(|_| false);
            snapshotter
                .expect_get_file_path()
                .returning(move |f| Path::new("/destination").join(f));

            snapshotter
                .expect_snapshot_subset()
                .times(1)
                .with(
                    eq(Path::new("cardano-database/immutable/00001.tar.gz")),
                    always(),
                )
                .returning(|_, _| Ok(dummy_ongoing_snapshot()));

            snapshotter
                .expect_snapshot_subset()
                .times(1)
                .with(
                    eq(Path::new("cardano-database/immutable/00002.tar.gz")),
                    always(),
                )
                .returning(|_, _| Ok(dummy_ongoing_snapshot()));
            snapshotter
        };

        let uploader = fake_uploader(
            vec![
                "/destination/cardano-database/immutable/00001.tar.gz",
                "/destination/cardano-database/immutable/00002.tar.gz",
            ],
            "archive.tar.gz",
        );

        let builder = ImmutableArtifactBuilder::new(
            vec![Arc::new(uploader)],
            Arc::new(snapshotter),
            CompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let archive_paths = builder.upload(2).await.unwrap();

        assert_equivalent(
            archive_paths,
            vec![ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri("archive.tar.gz".to_string())),
            }],
        )
    }

    mod create_archive {

        use super::*;

        #[test]
        fn snapshot_immutables_files_up_to_the_given_immutable_file_number() {
            let mut snapshotter = MockSnapshotter::new();
            snapshotter
                .expect_get_file_path()
                .returning(move |f| Path::new("/tmp").join(f));

            snapshotter
                .expect_does_snapshot_exist()
                .returning(|_| false);

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
                .returning(|_, _| Ok(dummy_ongoing_snapshot()));

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
                .returning(|_, _| Ok(dummy_ongoing_snapshot()));

            let builder = ImmutableArtifactBuilder::new(
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .immutable_archives_paths_creating_the_missing_ones(2)
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    PathBuf::from("/tmp/cardano-database/immutable/00001.tar.gz"),
                    PathBuf::from("/tmp/cardano-database/immutable/00002.tar.gz"),
                ],
            )
        }

        #[test]
        fn return_error_when_one_of_the_three_immutable_files_is_missing() {
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

            builder
                .immutable_archives_paths_creating_the_missing_ones(2)
                .expect_err(
                    "Should return an error when one of the three immutable files is missing",
                );
        }

        #[test]
        fn return_error_when_an_immutable_file_trio_is_missing() {
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
                .immutable_archives_paths_creating_the_missing_ones(3)
                .expect_err("Should return an error when an immutable file trio is missing");
        }

        #[test]
        fn return_error_when_immutable_file_number_is_not_produced_yet() {
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
                .immutable_archives_paths_creating_the_missing_ones(3)
                .expect_err("Should return an error when an immutable file trio is missing");
        }

        #[test]
        fn return_all_archives_but_not_rebuild_archives_already_compressed() {
            let mut snapshotter = MockSnapshotter::new();
            snapshotter
                .expect_get_file_path()
                .returning(move |f| Path::new("/tmp").join(f));

            snapshotter
                .expect_does_snapshot_exist()
                .times(1)
                .with(eq(Path::new("cardano-database/immutable/00001.tar.gz")))
                .returning(|_| true);

            snapshotter
                .expect_does_snapshot_exist()
                .times(1)
                .with(eq(Path::new("cardano-database/immutable/00002.tar.gz")))
                .returning(|_| true);

            snapshotter
                .expect_does_snapshot_exist()
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
                .returning(|_, _| Ok(dummy_ongoing_snapshot()));

            let builder = ImmutableArtifactBuilder::new(
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .immutable_archives_paths_creating_the_missing_ones(3)
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    PathBuf::from("/tmp/cardano-database/immutable/00001.tar.gz"),
                    PathBuf::from("/tmp/cardano-database/immutable/00002.tar.gz"),
                    PathBuf::from("/tmp/cardano-database/immutable/00003.tar.gz"),
                ],
            )
        }

        #[test]
        fn return_all_archives_paths_even_if_all_archives_already_exist() {
            let mut snapshotter = MockSnapshotter::new();
            snapshotter
                .expect_get_file_path()
                .returning(move |f| Path::new("/tmp").join(f));
            snapshotter.expect_does_snapshot_exist().returning(|_| true);
            snapshotter.expect_snapshot_subset().never();

            let builder = ImmutableArtifactBuilder::new(
                vec![Arc::new(MockImmutableFilesUploader::new())],
                Arc::new(snapshotter),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap();

            let archive_paths = builder
                .immutable_archives_paths_creating_the_missing_ones(3)
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    PathBuf::from("/tmp/cardano-database/immutable/00001.tar.gz"),
                    PathBuf::from("/tmp/cardano-database/immutable/00002.tar.gz"),
                    PathBuf::from("/tmp/cardano-database/immutable/00003.tar.gz"),
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
                .expect_batch_upload()
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
                    .upload_immutable_archives(&[
                        PathBuf::from("01.tar.gz"),
                        PathBuf::from("02.tar.gz"),
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
                .upload_immutable_archives(&[
                    PathBuf::from("01.tar.gz"),
                    PathBuf::from("02.tar.gz"),
                ])
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
                .upload_immutable_archives(&[
                    PathBuf::from("01.tar.gz"),
                    PathBuf::from("02.tar.gz"),
                ])
                .await
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![ImmutablesLocation::CloudStorage {
                    uri: MultiFilesUri::Template(TemplateUri("archive_2.tar.gz".to_string())),
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
                .upload_immutable_archives(&[
                    PathBuf::from("01.tar.gz"),
                    PathBuf::from("02.tar.gz"),
                ])
                .await
                .unwrap();

            assert_equivalent(
                archive_paths,
                vec![
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri("archive_1.tar.gz".to_string())),
                    },
                    ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri("archive_2.tar.gz".to_string())),
                    },
                ],
            )
        }
    }

    mod batch_upload {
        use std::fs::File;
        use std::io::Write;

        use mithril_common::test_utils::TempDir;
        use reqwest::Url;

        use super::*;

        fn create_fake_archive(dir: &Path, name: &str) -> PathBuf {
            let file_path = dir.join(name);
            let mut file = File::create(&file_path).unwrap();
            writeln!(
                file,
                "I swear, this is an archive, not a temporary test file."
            )
            .unwrap();

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

            let url_prefix = Url::parse("http://test.com:8080/base-root").unwrap();
            let uploader =
                LocalUploader::new(url_prefix, &target_dir, TestLogger::stdout()).unwrap();
            let location = ImmutableFilesUploader::batch_upload(
                &uploader,
                &[archive_1.clone(), archive_2.clone()],
            )
            .await
            .expect("local upload should not fail");

            assert!(target_dir.join(archive_1.file_name().unwrap()).exists());
            assert!(target_dir.join(archive_2.file_name().unwrap()).exists());

            let expected_location = ImmutablesLocation::CloudStorage {
                uri: MultiFilesUri::Template(TemplateUri(
                "http://test.com:8080/base-root/artifact/snapshot/{immutable_file_number}.tar.gz"
                    .to_string(),
            ))};
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

            let url_prefix = Url::parse("http://test.com:8080/base-root").unwrap();
            let uploader =
                LocalUploader::new(url_prefix, &target_dir, TestLogger::stdout()).unwrap();

            ImmutableFilesUploader::batch_upload(&uploader, &[archive])
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
}
