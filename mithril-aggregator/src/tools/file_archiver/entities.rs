use std::path::PathBuf;

use crate::ZstandardCompressionParameters;

/// Compression algorithm and parameters of the [crate::services::CompressedArchiveSnapshotter].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileArchiverCompressionAlgorithm {
    /// Gzip compression format
    Gzip,
    /// Zstandard compression format
    Zstandard(ZstandardCompressionParameters),
}

impl From<ZstandardCompressionParameters> for FileArchiverCompressionAlgorithm {
    fn from(params: ZstandardCompressionParameters) -> Self {
        Self::Zstandard(params)
    }
}

/// Result of a file archiving operation, containing the path to the archive and its size.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileArchive {
    pub(super) filepath: PathBuf,
    pub(super) filesize: u64,
}

impl FileArchive {
    /// `FileArchive` factory
    pub fn new(filepath: PathBuf, filesize: u64) -> Self {
        Self { filepath, filesize }
    }

    /// Get the path of the archive.
    pub fn get_file_path(&self) -> &PathBuf {
        &self.filepath
    }

    /// Get the size of the archive.
    pub fn get_file_size(&self) -> &u64 {
        &self.filesize
    }
}
