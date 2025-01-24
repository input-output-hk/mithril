use std::path::{Path, PathBuf};
use std::sync::RwLock;

use mithril_common::StdResult;

use crate::services::{OngoingSnapshot, SnapshotError, Snapshotter};

/// Snapshotter that does nothing. It is mainly used for test purposes.
pub struct DumbSnapshotter {
    last_snapshot: RwLock<Option<OngoingSnapshot>>,
}

impl DumbSnapshotter {
    /// Create a new instance of DumbSnapshotter.
    pub fn new() -> Self {
        Self {
            last_snapshot: RwLock::new(None),
        }
    }

    /// Return the last fake snapshot produced.
    pub fn get_last_snapshot(&self) -> StdResult<Option<OngoingSnapshot>> {
        let value = self
            .last_snapshot
            .read()
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?
            .as_ref()
            .cloned();

        Ok(value)
    }
}

impl Default for DumbSnapshotter {
    fn default() -> Self {
        Self::new()
    }
}

impl Snapshotter for DumbSnapshotter {
    fn snapshot_all(&self, archive_name: &Path) -> StdResult<OngoingSnapshot> {
        let mut value = self
            .last_snapshot
            .write()
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?;
        let snapshot = OngoingSnapshot {
            filepath: archive_name.to_path_buf(),
            filesize: 0,
        };
        *value = Some(snapshot.clone());

        Ok(snapshot)
    }

    fn snapshot_subset(
        &self,
        archive_name: &Path,
        _files: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot> {
        self.snapshot_all(archive_name)
    }

    fn does_snapshot_exist(&self, _filepath: &Path) -> bool {
        false
    }

    fn get_file_path(&self, filepath: &Path) -> PathBuf {
        filepath.to_path_buf()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dumb_snapshotter_snapshot_return_archive_name_with_size_0() {
        let snapshotter = DumbSnapshotter::new();
        let snapshot = snapshotter
            .snapshot_all(Path::new("archive.tar.gz"))
            .unwrap();

        assert_eq!(PathBuf::from("archive.tar.gz"), *snapshot.get_file_path());
        assert_eq!(0, *snapshot.get_file_size());

        let snapshot = snapshotter
            .snapshot_subset(Path::new("archive.tar.gz"), vec![PathBuf::from("whatever")])
            .unwrap();
        assert_eq!(PathBuf::from("archive.tar.gz"), *snapshot.get_file_path());
        assert_eq!(0, *snapshot.get_file_size());
    }

    #[test]
    fn test_dumb_snapshotter() {
        let snapshotter = DumbSnapshotter::new();
        assert!(snapshotter
            .get_last_snapshot()
            .expect("Dumb snapshotter::get_last_snapshot should not fail when no last snapshot.")
            .is_none());

        let snapshot = snapshotter
            .snapshot_all(Path::new("whatever"))
            .expect("Dumb snapshotter::snapshot should not fail.");
        assert_eq!(
            Some(snapshot),
            snapshotter.get_last_snapshot().expect(
                "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
            )
        );

        let snapshot = snapshotter
            .snapshot_subset(Path::new("another_whatever"), vec![PathBuf::from("subdir")])
            .expect("Dumb snapshotter::snapshot should not fail.");
        assert_eq!(
            Some(snapshot),
            snapshotter.get_last_snapshot().expect(
                "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
            )
        );
    }
}
