use std::fs;
use std::fs::File;
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
}

/// Snapshotter that writes empty files to the filesystem. Used for testing purposes.
pub struct FakeSnapshotter {
    work_dir: PathBuf,
}

impl FakeSnapshotter {
    /// `FakeSnapshotter` factory
    pub fn new<T: AsRef<Path>>(work_dir: T) -> Self {
        Self {
            work_dir: work_dir.as_ref().to_path_buf(),
        }
    }
}

impl Snapshotter for FakeSnapshotter {
    fn snapshot_all(&self, filepath: &Path) -> StdResult<OngoingSnapshot> {
        let fake_archive_path = self.work_dir.join(filepath);
        if let Some(archive_dir) = fake_archive_path.parent() {
            fs::create_dir_all(archive_dir).unwrap();
        }
        File::create(&fake_archive_path).unwrap();

        Ok(OngoingSnapshot {
            filepath: fake_archive_path,
            filesize: 0,
        })
    }

    fn snapshot_subset(&self, filepath: &Path, _files: Vec<PathBuf>) -> StdResult<OngoingSnapshot> {
        self.snapshot_all(filepath)
    }
}

#[cfg(test)]
mod tests {
    use crate::services::snapshotter::test_tools::*;

    use super::*;

    mod dumb_snapshotter {
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
                .expect(
                    "Dumb snapshotter::get_last_snapshot should not fail when no last snapshot."
                )
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

    mod fake_snapshotter {
        use super::*;
        #[test]
        fn snapshot_all_create_empty_file_located_at_work_dir_joined_filepath() {
            let test_dir = get_test_directory("fake_snapshotter_snapshot_all_create_empty_file");
            let fake_snapshotter = FakeSnapshotter::new(&test_dir);

            for path in [
                Path::new("direct_child.tar.gz"),
                Path::new("one_level_subdir/child.tar.gz"),
                Path::new("two_levels/subdir/child.tar.gz"),
            ] {
                let snapshot = fake_snapshotter.snapshot_all(path).unwrap();

                assert!(test_dir.join(path).is_file());
                assert_eq!(snapshot.get_file_path(), &test_dir.join(path));
            }
        }

        #[test]
        fn snapshot_subset_create_empty_file_located_at_work_dir_joined_filepath() {
            let test_dir = get_test_directory("fake_snapshotter_snapshot_subset_create_empty_file");
            let fake_snapshotter = FakeSnapshotter::new(&test_dir);

            for path in [
                Path::new("direct_child.tar.gz"),
                Path::new("one_level_subdir/child.tar.gz"),
                Path::new("two_levels/subdir/child.tar.gz"),
            ] {
                let snapshot = fake_snapshotter.snapshot_subset(path, vec![]).unwrap();

                assert!(test_dir.join(path).is_file());
                assert_eq!(snapshot.get_file_path(), &test_dir.join(path));
            }
        }
    }
}
