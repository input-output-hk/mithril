use anyhow::{anyhow, Context};
use mithril_common::entities::CompressionAlgorithm;
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;
use slog::Logger;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use crate::dependency_injection::DependenciesBuilderError;
use crate::tools::file_archiver::appender::{AppenderDirAll, AppenderEntries, TarAppender};
use crate::tools::file_archiver::{ArchiveParameters, FileArchiver};

use super::{OngoingSnapshot, Snapshotter};

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
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<OngoingSnapshot> {
        let appender = AppenderDirAll::new(self.db_directory.clone());

        self.snapshot(archive_name_without_extension, appender)
    }

    fn snapshot_subset(
        &self,
        archive_name_without_extension: &str,
        entries: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot> {
        if entries.is_empty() {
            return Err(anyhow!("Can not create snapshot with empty entries"));
        }

        let appender = AppenderEntries::new(entries, self.db_directory.clone())?;
        self.snapshot(archive_name_without_extension, appender)
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
    ) -> StdResult<OngoingSnapshot> {
        self.file_archiver
            .archive(
                ArchiveParameters {
                    archive_name_without_extension: name_without_extension.to_string(),
                    target_directory: self.ongoing_snapshot_directory.clone(),
                    compression_algorithm: self.compression_algorithm,
                },
                appender,
            )
            .map(|f| OngoingSnapshot {
                filepath: f.get_file_path().to_path_buf(),
                filesize: f.get_file_size(),
            })
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::sync::Arc;

    use mithril_common::test_utils::assert_equivalent;

    use crate::services::snapshotter::test_tools::*;
    use crate::test_tools::TestLogger;

    use super::*;

    #[test]
    fn should_create_directory_if_does_not_exist() {
        let test_dir = get_test_directory("should_create_directory_if_does_not_exist");
        let ongoing_snapshot_directory = test_dir.join("pending_snapshot");
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
    fn should_clean_pending_snapshot_directory_if_already_exists() {
        let test_dir =
            get_test_directory("should_clean_pending_snapshot_directory_if_already_exists");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("whatever");

        fs::create_dir_all(&pending_snapshot_directory).unwrap();

        File::create(pending_snapshot_directory.join("whatever.txt")).unwrap();

        CompressedArchiveSnapshotter::new(
            db_directory,
            pending_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        assert_eq!(0, fs::read_dir(pending_snapshot_directory).unwrap().count());
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

    #[test]
    fn should_delete_tmp_file_in_pending_snapshot_directory_if_snapshotting_fail() {
        let test_dir = get_test_directory(
            "should_delete_tmp_file_in_pending_snapshot_directory_if_snapshotting_fail",
        );
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("db");

        let snapshotter = CompressedArchiveSnapshotter::new(
            db_directory,
            pending_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        // this file should not be deleted by the archive creation
        File::create(pending_snapshot_directory.join("other-process.file")).unwrap();

        let _ = snapshotter
            .snapshot_all("whatever")
            .expect_err("Snapshotter::snapshot should fail if the db is empty.");
        let remaining_files: Vec<String> = fs::read_dir(&pending_snapshot_directory)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect();

        assert_eq!(vec!["other-process.file".to_string()], remaining_files);
    }

    #[test]
    fn should_not_delete_an_already_existing_archive_with_same_name_if_snapshotting_fail() {
        let test_dir = get_test_directory(
            "should_not_delete_an_already_existing_archive_with_same_name_if_snapshotting_fail",
        );
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("db");

        let snapshotter = CompressedArchiveSnapshotter::new(
            db_directory,
            pending_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        // this file should not be deleted by the archive creation
        create_file(&pending_snapshot_directory, "other-process.file");
        create_file(&pending_snapshot_directory, "whatever.tar.gz");
        // an already existing temporary archive file should be deleted
        create_file(&pending_snapshot_directory, "whatever.tar.tmp");

        let _ = snapshotter
            .snapshot_all("whatever")
            .expect_err("Snapshotter::snapshot should fail if the db is empty.");
        let remaining_files: Vec<String> = fs::read_dir(&pending_snapshot_directory)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect();

        assert_equivalent(
            vec![
                "other-process.file".to_string(),
                "whatever.tar.gz".to_string(),
            ],
            remaining_files,
        );
    }

    #[test]
    fn snapshot_overwrite_archive_already_existing() {
        let test_dir = get_test_directory("snapshot_overwrite_archive_already_existing");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        create_file(&source, "file_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            source.clone(),
            destination,
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        let first_snapshot = snapshotter.snapshot_all("whatever").unwrap();
        let first_snapshot_size = first_snapshot.get_file_size();

        create_file(&source, "another_file_to_archive.txt");

        let second_snapshot = snapshotter.snapshot_all("whatever").unwrap();
        let second_snapshot_size = second_snapshot.get_file_size();

        assert_ne!(first_snapshot_size, second_snapshot_size);

        let unpack_path = unpack_gz_decoder(test_dir, second_snapshot);
        assert!(unpack_path.join("another_file_to_archive.txt").exists());
    }
}
