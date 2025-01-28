use std::io;
use std::path::{Path, PathBuf};
use thiserror::Error;

use mithril_common::StdResult;

use crate::ZstandardCompressionParameters;

#[cfg_attr(test, mockall::automock)]
/// Define the ability to create snapshots.
pub trait Snapshotter: Sync + Send {
    /// Create a new snapshot with the given filepath.
    fn snapshot_all(&self, filepath: &Path) -> StdResult<OngoingSnapshot>;

    /// Create a new snapshot with the given filepath from a subset of directories and files.
    fn snapshot_subset(&self, filepath: &Path, files: Vec<PathBuf>) -> StdResult<OngoingSnapshot>;
}

/// Snapshotter error type.
#[derive(Error, Debug)]
pub enum SnapshotError {
    /// Set when the snapshotter fails at creating a snapshot.
    #[error("Create archive error: {0}")]
    CreateArchiveError(#[from] io::Error),

    /// Set when the snapshotter creates an invalid snapshot.
    #[error("Invalid archive error: {0}")]
    InvalidArchiveError(String),

    /// Set when the snapshotter fails verifying a snapshot.
    #[error("Archive verification error: {0}")]
    VerifyArchiveError(String),

    /// Set when the snapshotter fails at uploading the snapshot.
    #[error("Upload file error: `{0}`")]
    UploadFileError(String),

    /// General error.
    #[error("Snapshot General Error: `{0}`")]
    GeneralError(String),
}

/// Compression algorithm and parameters of the [crate::services::CompressedArchiveSnapshotter].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SnapshotterCompressionAlgorithm {
    /// Gzip compression format
    Gzip,
    /// Zstandard compression format
    Zstandard(ZstandardCompressionParameters),
}

impl From<ZstandardCompressionParameters> for SnapshotterCompressionAlgorithm {
    fn from(params: ZstandardCompressionParameters) -> Self {
        Self::Zstandard(params)
    }
}

/// An ongoing snapshot is a snapshot that is not yet uploaded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OngoingSnapshot {
    pub(super) filepath: PathBuf,
    pub(super) filesize: u64,
}

impl OngoingSnapshot {
    /// `OngoingSnapshot` factory
    pub fn new(filepath: PathBuf, filesize: u64) -> Self {
        Self { filepath, filesize }
    }

    /// Get the path of the snapshot archive.
    pub fn get_file_path(&self) -> &PathBuf {
        &self.filepath
    }

    /// Get the size of the snapshot archive.
    pub fn get_file_size(&self) -> &u64 {
        &self.filesize
    }
}
