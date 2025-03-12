use anyhow::{anyhow, Context};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use mithril_common::StdResult;

/// Define multiple ways to append content to a tar archive.
pub trait TarAppender {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()>;
}

pub struct AppenderDirAll {
    target_directory: PathBuf,
}

impl AppenderDirAll {
    pub fn new(target_directory: PathBuf) -> Self {
        Self { target_directory }
    }
}

impl TarAppender for AppenderDirAll {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        tar.append_dir_all(".", &self.target_directory)
            .with_context(|| {
                format!(
                    "Create archive error:  Can not add directory: '{}' to the archive",
                    self.target_directory.display()
                )
            })?;
        Ok(())
    }
}

pub struct AppenderFile {
    /// Location of the file in the archive.
    location_in_archive: PathBuf,
    /// Path to the file to add to the archive.
    target_file: PathBuf,
}

impl AppenderFile {
    /// Append the file at the root of the archive, keeping the same file name.
    pub fn append_at_archive_root(target_file: PathBuf) -> StdResult<Self> {
        if !target_file.is_file() {
            return Err(anyhow!(
                "The target file is not a file, path: {}",
                target_file.display()
            ));
        }

        let location_in_archive = target_file
            .file_name()
            .ok_or_else(|| {
                anyhow!(
                    "Can not get the file name from the target file path: '{}'",
                    target_file.display()
                )
            })?
            .to_owned();

        Ok(Self {
            location_in_archive: PathBuf::from(location_in_archive),
            target_file,
        })
    }
}

impl TarAppender for AppenderFile {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        let mut file = File::open(&self.target_file)
            .with_context(|| format!("Can not open file: '{}'", self.target_file.display()))?;
        tar.append_file(&self.location_in_archive, &mut file)
            .with_context(|| {
                format!(
                    "Can not add file: '{}' to the archive",
                    self.target_file.display()
                )
            })?;
        Ok(())
    }
}

pub struct AppenderEntries {
    entries: Vec<PathBuf>,
    base_directory: PathBuf,
}

impl AppenderEntries {
    /// Create a new instance of `AppenderEntries`.
    ///
    /// Returns an error if the `entries` are empty.
    pub fn new(entries: Vec<PathBuf>, base_directory: PathBuf) -> StdResult<Self> {
        if entries.is_empty() {
            return Err(anyhow!("The entries can not be empty"));
        }

        Ok(Self {
            entries,
            base_directory,
        })
    }
}

impl TarAppender for AppenderEntries {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        for entry in &self.entries {
            let entry_path = self.base_directory.join(entry);
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
    use mithril_common::entities::CompressionAlgorithm;

    use crate::tools::file_archiver::test_tools::*;
    use crate::tools::file_archiver::{ArchiveParameters, FileArchiver};

    use super::*;

    #[test]
    fn snapshot_subset_should_create_archive_only_for_specified_directories_and_files() {
        let test_dir = get_test_directory("only_for_specified_directories_and_files");
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "file_to_archive.txt");
        let directory_not_to_archive_path = create_dir(&source, "directory_not_to_archive");
        let file_not_to_archive_path = create_file(&source, "file_not_to_archive.txt");

        let file_archiver = FileArchiver::new_for_test(test_dir.join("verification"));

        let snapshot = file_archiver
            .archive(
                ArchiveParameters {
                    archive_name_without_extension: "archive".to_string(),
                    target_directory: test_dir.clone(),
                    compression_algorithm: CompressionAlgorithm::Gzip,
                },
                AppenderEntries::new(
                    vec![
                        directory_to_archive_path.clone(),
                        file_to_archive_path.clone(),
                    ],
                    source,
                )
                .unwrap(),
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(&test_dir, snapshot);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
        assert!(!unpack_path.join(directory_not_to_archive_path).exists());
        assert!(!unpack_path.join(file_not_to_archive_path).exists());
    }

    #[test]
    fn snapshot_subset_return_error_when_file_or_directory_not_exist() {
        let test_dir = get_test_directory("file_or_directory_not_exist");
        let target_archive = test_dir.join("whatever.tar.gz");
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let file_archiver = FileArchiver::new_for_test(test_dir.join("verification"));

        file_archiver
            .archive(
                ArchiveParameters {
                    archive_name_without_extension: "archive".to_string(),
                    target_directory: test_dir.clone(),
                    compression_algorithm: CompressionAlgorithm::Gzip,
                },
                AppenderEntries::new(vec![PathBuf::from("not_exist")], source).unwrap(),
            )
            .expect_err("snapshot_subset should return error when file or directory not exist");
        assert!(!target_archive.exists());
    }

    #[test]
    fn snapshot_subset_return_error_when_empty_entries() {
        let appender_creation_result = AppenderEntries::new(vec![], PathBuf::new());
        assert!(appender_creation_result.is_err(),);
    }

    #[test]
    fn snapshot_subset_with_duplicate_files_and_directories() {
        let test_dir = get_test_directory("with_duplicate_files_and_directories");
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "directory_to_archive/file_to_archive.txt");

        let file_archiver = FileArchiver::new_for_test(test_dir.join("verification"));

        let snapshot = file_archiver
            .archive(
                ArchiveParameters {
                    archive_name_without_extension: "archive".to_string(),
                    target_directory: test_dir.clone(),
                    compression_algorithm: CompressionAlgorithm::Gzip,
                },
                AppenderEntries::new(
                    vec![
                        directory_to_archive_path.clone(),
                        directory_to_archive_path.clone(),
                        file_to_archive_path.clone(),
                        file_to_archive_path.clone(),
                    ],
                    source,
                )
                .unwrap(),
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(&test_dir, snapshot);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
    }

    #[test]
    fn appender_file_should_append_file_to_tar() {
        let test_dir = get_test_directory("appender_file_should_append_file_to_tar");
        let file_to_archive = create_file(&test_dir, "test_file.txt");

        let file_archiver = FileArchiver::new_for_test(test_dir.join("verification"));
        let archive = file_archiver
            .archive(
                ArchiveParameters {
                    archive_name_without_extension: "archive".to_string(),
                    target_directory: test_dir.clone(),
                    compression_algorithm: CompressionAlgorithm::Gzip,
                },
                AppenderFile::append_at_archive_root(test_dir.join(&file_to_archive)).unwrap(),
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(&test_dir, archive);

        assert!(unpack_path.join(file_to_archive).exists());
    }

    #[test]
    fn appender_file_should_return_error_if_file_does_not_exist() {
        let target_file_path = PathBuf::from("non_existent_file.txt");
        assert!(AppenderFile::append_at_archive_root(target_file_path).is_err());
    }

    #[test]
    fn appender_file_should_return_error_if_input_is_not_a_file() {
        let test_dir =
            get_test_directory("appender_file_should_return_error_if_input_is_not_a_file");
        assert!(AppenderFile::append_at_archive_root(test_dir).is_err());
    }
}
