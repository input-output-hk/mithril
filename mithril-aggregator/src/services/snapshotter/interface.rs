use std::path::PathBuf;

use mithril_common::StdResult;

use crate::tools::file_archiver::FileArchive;
use crate::ZstandardCompressionParameters;

#[cfg_attr(test, mockall::automock)]
/// Define the ability to create snapshots.
pub trait Snapshotter: Sync + Send {
    /// Create a new snapshot with the given filepath.
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<FileArchive>;

    /// Create a new snapshot with the given filepath from a subset of directories and files.
    fn snapshot_subset(
        &self,
        archive_name_without_extension: &str,
        files: Vec<PathBuf>,
    ) -> StdResult<FileArchive>;
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
