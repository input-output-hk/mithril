use anyhow::{anyhow, Context};
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use mithril_common::StdResult;

use crate::tools::file_size;

/// Define multiple ways to append content to a tar archive.
pub trait TarAppender: Send {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()>;

    fn compute_uncompressed_data_size(&self) -> StdResult<u64>;
}

pub struct AppenderDirAll {
    target_directory: PathBuf,
}

impl AppenderDirAll {
    // Note: Not used anymore outside of tests but useful tool to keep around if we ever need to archive a directory
    #[cfg(test)]
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

    fn compute_uncompressed_data_size(&self) -> StdResult<u64> {
        file_size::compute_size_of_path(&self.target_directory)
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

    fn compute_uncompressed_data_size(&self) -> StdResult<u64> {
        file_size::compute_size_of_path(&self.target_file)
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

    fn compute_uncompressed_data_size(&self) -> StdResult<u64> {
        let full_entries_path = self
            .entries
            .iter()
            .map(|entry| self.base_directory.join(entry))
            .collect();
        file_size::compute_size(full_entries_path)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::digesters::{
        DummyCardanoDbBuilder, IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR,
    };
    use mithril_common::entities::CompressionAlgorithm;

    use crate::tools::file_archiver::test_tools::*;
    use crate::tools::file_archiver::{ArchiveParameters, FileArchiver};

    use super::*;

    #[test]
    fn appender_entries_should_create_archive_only_for_specified_directories_and_files() {
        let test_dir = get_test_directory("only_for_specified_directories_and_files");
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "file_to_archive.txt");
        let directory_not_to_archive_path = create_dir(&source, "directory_not_to_archive");
        let file_not_to_archive_path = create_file(&source, "file_not_to_archive.txt");

        let file_archiver = FileArchiver::new_for_test(test_dir.join("verification"));

        let archive = file_archiver
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

        let unpack_path = archive.unpack_gzip(&test_dir);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
        assert!(!unpack_path.join(directory_not_to_archive_path).exists());
        assert!(!unpack_path.join(file_not_to_archive_path).exists());
    }

    #[test]
    fn appender_entries_return_error_when_file_or_directory_not_exist() {
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
            .expect_err("AppenderEntries should return error when file or directory not exist");
        assert!(!target_archive.exists());
    }

    #[test]
    fn appender_entries_return_error_when_empty_entries() {
        let appender_creation_result = AppenderEntries::new(vec![], PathBuf::new());
        assert!(appender_creation_result.is_err(),);
    }

    #[test]
    fn appender_entries_with_duplicate_files_and_directories() {
        let test_dir = get_test_directory("with_duplicate_files_and_directories");
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "directory_to_archive/file_to_archive.txt");

        let file_archiver = FileArchiver::new_for_test(test_dir.join("verification"));

        let archive = file_archiver
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

        let unpack_path = archive.unpack_gzip(&test_dir);

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

        let unpack_path = archive.unpack_gzip(&test_dir);

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

    #[test]
    fn appender_dir_all_compute_size() {
        let test_dir = "appender_dir_all_compute_size";

        let immutable_trio_file_size = 777;
        let ledger_file_size = 6666;
        let volatile_file_size = 99;

        let cardano_db = DummyCardanoDbBuilder::new(test_dir)
            .with_immutables(&[1, 2])
            .set_immutable_trio_file_size(immutable_trio_file_size)
            .with_ledger_files(&["437", "537", "637"])
            .set_ledger_file_size(ledger_file_size)
            .with_volatile_files(&["blocks-0.dat"])
            .set_volatile_file_size(volatile_file_size)
            .build();

        let appender_dir_all = AppenderDirAll::new(cardano_db.get_dir().clone());

        let entries_size = appender_dir_all.compute_uncompressed_data_size().unwrap();
        let expected_total_size =
            (immutable_trio_file_size * 2) + (3 * ledger_file_size) + volatile_file_size;
        assert_eq!(expected_total_size, entries_size);
    }

    #[test]
    fn appender_file_all_compute_size() {
        let test_dir = get_test_directory("appender_file_all_compute_size");

        let file_path = test_dir.join("file.txt");
        let file = File::create(&file_path).unwrap();
        file.set_len(777).unwrap();

        let appender_file = AppenderFile::append_at_archive_root(file_path).unwrap();

        let entries_size = appender_file.compute_uncompressed_data_size().unwrap();
        assert_eq!(777, entries_size);
    }

    #[test]
    fn appender_entries_compute_size_of_its_paths() {
        let test_dir = "appender_entries_compute_size_of_its_paths";

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

        let appender_entries = AppenderEntries::new(
            vec![
                PathBuf::from(IMMUTABLE_DIR),
                PathBuf::from(LEDGER_DIR).join("437"),
                PathBuf::from(LEDGER_DIR).join("537"),
                PathBuf::from(VOLATILE_DIR).join("blocks-0.dat"),
            ],
            cardano_db.get_dir().clone(),
        )
        .unwrap();

        let entries_size = appender_entries.compute_uncompressed_data_size().unwrap();
        let expected_total_size =
            (immutable_trio_file_size * 3) + (2 * ledger_file_size) + volatile_file_size;
        assert_eq!(expected_total_size, entries_size);
    }
}
