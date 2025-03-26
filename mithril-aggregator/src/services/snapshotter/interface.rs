use async_trait::async_trait;

use mithril_common::entities::{CompressionAlgorithm, ImmutableFileNumber};
use mithril_common::StdResult;

use crate::tools::file_archiver::FileArchive;

#[cfg_attr(test, mockall::automock)]
#[async_trait]
/// Define the ability to create snapshots.
pub trait Snapshotter: Sync + Send {
    /// Create a new snapshot containing all completed immutables.
    async fn snapshot_all_completed_immutables(
        &self,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive>;

    /// Create a new snapshot of ancillary files.
    ///
    /// Ancillary files include the last, uncompleted, immutable trio and the last ledger file.
    async fn snapshot_ancillary(
        &self,
        immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive>;

    /// Create a new snapshot of an immutable trio.
    async fn snapshot_immutable_trio(
        &self,
        immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive>;

    /// Return the compression algorithm used by the snapshotter.
    fn compression_algorithm(&self) -> CompressionAlgorithm;
}
