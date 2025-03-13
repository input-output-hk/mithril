use std::path::{Path, PathBuf};

use mithril_common::entities::CompressionAlgorithm;

/// Parameters for creating an archive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArchiveParameters {
    pub archive_name_without_extension: String,
    pub target_directory: PathBuf,
    pub compression_algorithm: CompressionAlgorithm,
}

impl ArchiveParameters {
    pub(super) fn target_path(&self) -> PathBuf {
        self.target_directory.join(format!(
            "{}.{}",
            self.archive_name_without_extension,
            self.compression_algorithm.tar_file_extension()
        ))
    }

    pub(super) fn temporary_archive_path(&self) -> PathBuf {
        self.target_directory
            .join(format!("{}.tar.tmp", self.archive_name_without_extension))
    }
}

/// Result of a file archiving operation, containing the path to the archive and its size.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileArchive {
    pub(super) filepath: PathBuf,
    // Todo: rename to `archive_size` and add `uncompressed_size` field.
    pub(super) filesize: u64,
    pub(super) compression_algorithm: CompressionAlgorithm,
}

impl FileArchive {
    /// Create a new instance of FileArchive.
    pub fn new(
        filepath: PathBuf,
        filesize: u64,
        compression_algorithm: CompressionAlgorithm,
    ) -> Self {
        Self {
            filepath,
            filesize,
            compression_algorithm,
        }
    }

    /// Get the path of the archive.
    pub fn get_file_path(&self) -> &Path {
        &self.filepath
    }

    /// Get the size of the archive.
    pub fn get_file_size(&self) -> u64 {
        self.filesize
    }

    /// Get the compression algorithm used to create the archive.
    pub fn get_compression_algorithm(&self) -> CompressionAlgorithm {
        self.compression_algorithm
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn getting_archive_parameters_target_path_should_not_override_trailing_dot_text() {
        let archive_parameters = ArchiveParameters {
            archive_name_without_extension: "archive.test_xxx".to_owned(),
            target_directory: PathBuf::from("/tmp"),
            compression_algorithm: CompressionAlgorithm::Gzip,
        };

        assert_eq!(
            PathBuf::from("/tmp/archive.test_xxx.tar.gz"),
            archive_parameters.target_path()
        );
    }

    #[test]
    fn getting_archive_parameters_temporary_archive_path_should_not_override_trailing_dot_text() {
        let archive_parameters = ArchiveParameters {
            archive_name_without_extension: "archive.test_xxx".to_owned(),
            target_directory: PathBuf::from("/tmp"),
            compression_algorithm: CompressionAlgorithm::Gzip,
        };

        assert_eq!(
            PathBuf::from("/tmp/archive.test_xxx.tar.tmp"),
            archive_parameters.temporary_archive_path()
        );
    }
}
