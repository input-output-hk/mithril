use anyhow::{anyhow, Context};
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;
use slog::Logger;
use std::fs;
use std::path::{Path, PathBuf};

use super::{OngoingSnapshot, Snapshotter};
use crate::dependency_injection::DependenciesBuilderError;
use crate::tools::file_archiver::appender::{AppenderDirAll, AppenderEntries, TarAppender};
use crate::tools::file_archiver::{FileArchiver, FileArchiverCompressionAlgorithm};

/// Compressed Archive Snapshotter create a compressed file.
pub struct CompressedArchiveSnapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store ongoing snapshot
    ongoing_snapshot_directory: PathBuf,

    file_archiver: FileArchiver,

    logger: Logger,
}

impl Snapshotter for CompressedArchiveSnapshotter {
    fn snapshot_all(&self, filepath: &Path) -> StdResult<OngoingSnapshot> {
        let appender = AppenderDirAll::new(self.db_directory.clone());

        self.snapshot(filepath, appender)
    }

    fn snapshot_subset(
        &self,
        filepath: &Path,
        entries: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot> {
        if entries.is_empty() {
            return Err(anyhow!("Can not create snapshot with empty entries"));
        }

        let appender = AppenderEntries::new(entries, self.db_directory.clone())?;
        self.snapshot(filepath, appender)
    }
}

impl CompressedArchiveSnapshotter {
    /// Snapshotter factory
    pub fn new(
        db_directory: PathBuf,
        ongoing_snapshot_directory: PathBuf,
        compression_algorithm: FileArchiverCompressionAlgorithm,
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
        let file_archiver =
            FileArchiver::new(compression_algorithm, std::env::temp_dir(), logger.clone());

        Ok(Self {
            db_directory,
            ongoing_snapshot_directory,
            file_archiver,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    #[cfg(test)]
    /// Allow to use a custom temporary directory to avoid conflicts during the snapshot verification.
    pub fn set_sub_temp_dir<T: Into<String>>(&mut self, sub_dir: T) {
        self.file_archiver.set_verification_temp_dir(sub_dir);
    }

    fn snapshot<T: TarAppender>(&self, filepath: &Path, appender: T) -> StdResult<OngoingSnapshot> {
        let archive_path = self.ongoing_snapshot_directory.join(filepath);
        self.file_archiver
            .archive(&archive_path, appender)
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
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("whatever");

        Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                FileArchiverCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        assert!(pending_snapshot_directory.is_dir());
    }

    #[test]
    fn should_clean_pending_snapshot_directory_if_already_exists() {
        let test_dir =
            get_test_directory("should_clean_pending_snapshot_directory_if_already_exists");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("whatever");

        fs::create_dir_all(&pending_snapshot_directory).unwrap();

        File::create(pending_snapshot_directory.join("whatever.txt")).unwrap();

        Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                FileArchiverCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        assert_eq!(0, fs::read_dir(pending_snapshot_directory).unwrap().count());
    }

    #[test]
    fn should_create_snapshots_in_its_ongoing_snapshot_directory() {
        let test_dir =
            get_test_directory("should_create_snapshots_in_its_ongoing_snapshot_directory");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join(create_dir(&test_dir, "db"));

        create_file(&db_directory, "file_to_archive.txt");

        let snapshotter = Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                FileArchiverCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );
        let snapshot = snapshotter
            .snapshot_all(Path::new("whatever.tar.gz"))
            .unwrap();

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

        let snapshotter = Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                FileArchiverCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        // this file should not be deleted by the archive creation
        File::create(pending_snapshot_directory.join("other-process.file")).unwrap();

        let _ = snapshotter
            .snapshot_all(Path::new("whatever.tar.gz"))
            .expect_err("Snapshotter::snapshot should fail if the db is empty.");
        let remaining_files: Vec<String> = fs::read_dir(&pending_snapshot_directory)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect();

        assert_eq!(vec!["other-process.file".to_string()], remaining_files);
    }

    #[test]
    fn should_not_delete_an_alreay_existing_archive_with_same_name_if_snapshotting_fail() {
        let test_dir = get_test_directory(
            "should_not_delete_an_alreay_existing_archive_with_same_name_if_snapshotting_fail",
        );
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("db");

        let snapshotter = Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                FileArchiverCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        // this file should not be deleted by the archive creation
        create_file(&pending_snapshot_directory, "other-process.file");
        create_file(&pending_snapshot_directory, "whatever.tar.gz");
        // an already existing temporary archive file should be deleted
        create_file(&pending_snapshot_directory, "whatever.tar.tmp");

        let _ = snapshotter
            .snapshot_all(Path::new("whatever.tar.gz"))
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
            FileArchiverCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let first_snapshot = snapshotter
            .snapshot_all(Path::new(&random_archive_name()))
            .unwrap();
        let first_snapshot_size = first_snapshot.get_file_size();

        create_file(&source, "another_file_to_archive.txt");

        let second_snapshot = snapshotter
            .snapshot_all(Path::new(&random_archive_name()))
            .unwrap();
        let second_snapshot_size = second_snapshot.get_file_size();

        assert_ne!(first_snapshot_size, second_snapshot_size);

        let unpack_path = unpack_gz_decoder(test_dir, second_snapshot);
        assert!(unpack_path.join("another_file_to_archive.txt").exists());
    }
}
