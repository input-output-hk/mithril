use std::path::PathBuf;

use mithril_common::StdResult;

use crate::tools::file_archiver::FileArchive;

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
