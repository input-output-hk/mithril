use anyhow::{anyhow, Context};
use slog::{debug, Logger};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::digesters::{immutable_trio_names, IMMUTABLE_DIR};
use mithril_common::entities::{CompressionAlgorithm, ImmutableFileNumber};
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::dependency_injection::DependenciesBuilderError;
use crate::tools::file_archiver::appender::{AppenderDirAll, AppenderEntries, TarAppender};
use crate::tools::file_archiver::{ArchiveParameters, FileArchive, FileArchiver};

use super::Snapshotter;

/// Compressed Archive Snapshotter create a compressed file.
pub struct CompressedArchiveSnapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store ongoing snapshot
    ongoing_snapshot_directory: PathBuf,

    /// Compression algorithm to use
    compression_algorithm: CompressionAlgorithm,

    file_archiver: Arc<FileArchiver>,

    logger: Logger,
}

impl Snapshotter for CompressedArchiveSnapshotter {
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<FileArchive> {
        let appender = AppenderDirAll::new(self.db_directory.clone());

        self.snapshot(archive_name_without_extension, appender)
    }

    fn snapshot_subset(
        &self,
        archive_name_without_extension: &str,
        entries: Vec<PathBuf>,
    ) -> StdResult<FileArchive> {
        if entries.is_empty() {
            return Err(anyhow!("Can not create snapshot with empty entries"));
        }

        let appender = AppenderEntries::new(entries, self.db_directory.clone())?;
        self.snapshot(archive_name_without_extension, appender)
    }

    fn snapshot_immutable_trio(
        &self,
        immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        debug!(
            self.logger,
            "Snapshotting immutable trio {immutable_file_number} into archive '{archive_name_without_extension}'"
        );

        let files_to_archive = immutable_trio_names(immutable_file_number)
            .iter()
            .map(|filename| PathBuf::from(IMMUTABLE_DIR).join(filename))
            .collect();
        let appender = AppenderEntries::new(files_to_archive, self.db_directory.clone())?;

        self.snapshot(archive_name_without_extension, appender)
    }

    fn compression_algorithm(&self) -> CompressionAlgorithm {
        self.compression_algorithm
    }
}

impl CompressedArchiveSnapshotter {
    /// Snapshotter factory
    pub fn new(
        db_directory: PathBuf,
        ongoing_snapshot_directory: PathBuf,
        compression_algorithm: CompressionAlgorithm,
        file_archiver: Arc<FileArchiver>,
        logger: Logger,
    ) -> StdResult<CompressedArchiveSnapshotter> {
        if ongoing_snapshot_directory.exists() {
            fs::remove_dir_all(&ongoing_snapshot_directory).with_context(|| {
                format!(
                    "Can not remove snapshotter directory: '{}'.",
                    ongoing_snapshot_directory.display()
                )
            })?;
        }

        fs::create_dir(&ongoing_snapshot_directory).map_err(|e| {
            DependenciesBuilderError::Initialization {
                message: format!(
                    "Can not create snapshotter directory: '{}'.",
                    ongoing_snapshot_directory.display()
                ),
                error: Some(e.into()),
            }
        })?;

        Ok(Self {
            db_directory,
            ongoing_snapshot_directory,
            compression_algorithm,
            file_archiver,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    fn snapshot<T: TarAppender>(
        &self,
        name_without_extension: &str,
        appender: T,
    ) -> StdResult<FileArchive> {
        self.file_archiver.archive(
            ArchiveParameters {
                archive_name_without_extension: name_without_extension.to_string(),
                target_directory: self.ongoing_snapshot_directory.clone(),
                compression_algorithm: self.compression_algorithm,
            },
            appender,
        )
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::path::Path;
    use std::sync::Arc;

    use mithril_common::digesters::DummyCardanoDbBuilder;
    use mithril_common::test_utils::assert_equivalent;

    use crate::services::snapshotter::test_tools::*;
    use crate::test_tools::TestLogger;

    use super::*;

    fn list_files(test_dir: &Path) -> Vec<String> {
        fs::read_dir(test_dir)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect()
    }

    fn snapshotter_for_test(
        test_directory: &Path,
        db_directory: &Path,
        compression_algorithm: CompressionAlgorithm,
    ) -> CompressedArchiveSnapshotter {
        CompressedArchiveSnapshotter::new(
            db_directory.to_path_buf(),
            test_directory.join("ongoing_snapshot"),
            compression_algorithm,
            Arc::new(FileArchiver::new_for_test(
                test_directory.join("verification"),
            )),
            TestLogger::stdout(),
        )
        .unwrap()
    }

    #[test]
    fn return_parametrized_compression_algorithm() {
        let test_dir = get_test_directory("return_parametrized_compression_algorithm");
        let snapshotter = snapshotter_for_test(
            &test_dir,
            Path::new("whatever"),
            CompressionAlgorithm::Zstandard,
        );

        assert_eq!(
            CompressionAlgorithm::Zstandard,
            snapshotter.compression_algorithm()
        );
    }

    #[test]
    fn should_create_directory_if_does_not_exist() {
        let test_dir = get_test_directory("should_create_directory_if_does_not_exist");
        let ongoing_snapshot_directory = test_dir.join("ongoing_snapshot");
        let db_directory = test_dir.join("whatever");

        CompressedArchiveSnapshotter::new(
            db_directory,
            ongoing_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        assert!(ongoing_snapshot_directory.is_dir());
    }

    #[test]
    fn should_clean_ongoing_snapshot_directory_if_already_exists() {
        let test_dir =
            get_test_directory("should_clean_ongoing_snapshot_directory_if_already_exists");
        let ongoing_snapshot_directory = test_dir.join("ongoing_snapshot");
        let db_directory = test_dir.join("whatever");

        fs::create_dir_all(&ongoing_snapshot_directory).unwrap();

        File::create(ongoing_snapshot_directory.join("whatever.txt")).unwrap();

        CompressedArchiveSnapshotter::new(
            db_directory,
            ongoing_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        assert_eq!(0, fs::read_dir(ongoing_snapshot_directory).unwrap().count());
    }

    #[test]
    fn should_create_snapshots_in_its_ongoing_snapshot_directory() {
        let test_dir =
            get_test_directory("should_create_snapshots_in_its_ongoing_snapshot_directory");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join(create_dir(&test_dir, "db"));

        create_file(&db_directory, "file_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            db_directory,
            pending_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();
        let snapshot = snapshotter.snapshot_all("whatever").unwrap();

        assert_eq!(
            pending_snapshot_directory,
            snapshot.get_file_path().parent().unwrap()
        );
    }

    mod snapshot_immutable_trio {
        use super::*;

        #[test]
        fn include_only_immutable_trio() {
            let test_dir = get_test_directory("include_only_immutable_trio");
            let cardano_db = DummyCardanoDbBuilder::new("include_only_immutable_trio")
                .with_immutables(&[1, 2, 3])
                .with_ledger_files(&["437"])
                .with_volatile_files(&["blocks-0.dat"])
                .with_non_immutables(&["random_file.txt", "00002.trap"])
                .build();

            let snapshotter =
                snapshotter_for_test(&test_dir, cardano_db.get_dir(), CompressionAlgorithm::Gzip);

            let snapshot = snapshotter
                .snapshot_immutable_trio(2, "immutable-2")
                .unwrap();

            let unpack_dir = unpack_gz_decoder(test_dir, snapshot);
            let unpacked_files = list_files(&unpack_dir);
            let unpacked_immutable_files = list_files(&unpack_dir.join(IMMUTABLE_DIR));

            assert_equivalent(vec![IMMUTABLE_DIR.to_string()], unpacked_files);
            assert_equivalent(
                vec![
                    "00002.chunk".to_string(),
                    "00002.primary".to_string(),
                    "00002.secondary".to_string(),
                ],
                unpacked_immutable_files,
            );
        }
    }
}
