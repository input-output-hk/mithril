use anyhow::{anyhow, Context};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use mithril_common::StdResult;

use crate::services::SnapshotError;

/// Define multiple ways to append content to a tar archive.
pub(super) trait TarAppender {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()>;
}

pub(super) struct AppenderDirAll {
    pub db_directory: PathBuf,
}

impl TarAppender for AppenderDirAll {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        tar.append_dir_all(".", &self.db_directory)
            .map_err(SnapshotError::CreateArchiveError)
            .with_context(|| {
                format!(
                    "Can not add directory: '{}' to the archive",
                    self.db_directory.display()
                )
            })?;
        Ok(())
    }
}

pub(super) struct AppenderEntries {
    pub(super) entries: Vec<PathBuf>,
    pub(super) db_directory: PathBuf,
}

impl TarAppender for AppenderEntries {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        for entry in &self.entries {
            let entry_path = self.db_directory.join(entry);
            if entry_path.is_dir() {
                tar.append_dir_all(entry, entry_path.clone())
                    .with_context(|| {
                        format!(
                            "Can not add directory: '{}' to the archive",
                            entry_path.display()
                        )
                    })?;
            } else if entry_path.is_file() {
                let mut file = File::open(entry_path.clone())?;
                tar.append_file(entry, &mut file).with_context(|| {
                    format!(
                        "Can not add file: '{}' to the archive",
                        entry_path.display()
                    )
                })?;
            } else {
                return Err(anyhow!(
                    "The entry: '{}' is not valid",
                    entry_path.display()
                ));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::services::snapshotter::test_tools::*;
    use crate::services::{
        CompressedArchiveSnapshotter, Snapshotter, SnapshotterCompressionAlgorithm,
    };
    use crate::test_tools::TestLogger;

    use super::*;

    #[test]
    fn snapshot_subset_should_create_archive_only_for_specified_directories_and_files() {
        let test_dir = get_test_directory("only_for_specified_directories_and_files");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "file_to_archive.txt");
        let directory_not_to_archive_path = create_dir(&source, "directory_not_to_archive");
        let file_not_to_archive_path = create_file(&source, "file_not_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let snapshot = snapshotter
            .snapshot_subset(
                Path::new(&random_archive_name()),
                vec![
                    directory_to_archive_path.clone(),
                    file_to_archive_path.clone(),
                ],
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(test_dir, snapshot);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
        assert!(!unpack_path.join(directory_not_to_archive_path).exists());
        assert!(!unpack_path.join(file_not_to_archive_path).exists());
    }

    #[test]
    fn snapshot_subset_return_error_when_file_or_directory_not_exist() {
        let test_dir = get_test_directory("file_or_directory_not_exist");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        snapshotter
            .snapshot_subset(
                Path::new(&random_archive_name()),
                vec![PathBuf::from("not_exist")],
            )
            .expect_err("snapshot_subset should return error when file or directory not exist");
    }

    #[test]
    fn snapshot_subset_return_error_when_empty_entries() {
        let test_dir = get_test_directory("empty_entries");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        snapshotter
            .snapshot_subset(Path::new(&random_archive_name()), vec![])
            .expect_err("snapshot_subset should return error when entries is empty");
    }

    #[test]
    fn snapshot_subset_with_duplicate_files_and_directories() {
        let test_dir = get_test_directory("with_duplicate_files_and_directories");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "directory_to_archive/file_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let snapshot = snapshotter
            .snapshot_subset(
                Path::new(&random_archive_name()),
                vec![
                    directory_to_archive_path.clone(),
                    directory_to_archive_path.clone(),
                    file_to_archive_path.clone(),
                    file_to_archive_path.clone(),
                ],
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(test_dir, snapshot);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
    }
}
