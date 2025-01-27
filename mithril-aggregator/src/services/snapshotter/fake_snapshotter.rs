use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};

use mithril_common::StdResult;

use crate::services::{OngoingSnapshot, Snapshotter};

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
