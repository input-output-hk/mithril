use std::path::PathBuf;

use mithril_common::StdResult;

use crate::ZstandardCompressionParameters;

#[cfg_attr(test, mockall::automock)]
/// Define the ability to create snapshots.
pub trait Snapshotter: Sync + Send {
    /// Create a new snapshot with the given filepath.
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<OngoingSnapshot>;

    /// Create a new snapshot with the given filepath from a subset of directories and files.
    fn snapshot_subset(
        &self,
        archive_name_without_extension: &str,
        files: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot>;
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
