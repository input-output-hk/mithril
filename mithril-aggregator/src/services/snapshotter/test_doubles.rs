use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

use mithril_common::entities::CompressionAlgorithm;
use mithril_common::StdResult;

use crate::services::{OngoingSnapshot, SnapshotError, Snapshotter};

/// Snapshotter that does nothing. It is mainly used for test purposes.
pub struct DumbSnapshotter {
    last_snapshot: RwLock<Option<OngoingSnapshot>>,
    compression_algorithm: CompressionAlgorithm,
}

impl DumbSnapshotter {
    /// Create a new instance of DumbSnapshotter.
    ///
    /// The `compression_algorithm` parameter is used for the output file name extension.
    pub fn new(compression_algorithm: CompressionAlgorithm) -> Self {
        Self {
            last_snapshot: RwLock::new(None),
            compression_algorithm,
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
        Self {
            last_snapshot: RwLock::new(None),
            compression_algorithm: CompressionAlgorithm::Gzip,
        }
    }
}

impl Snapshotter for DumbSnapshotter {
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<OngoingSnapshot> {
        let mut value = self
            .last_snapshot
            .write()
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?;
        let snapshot = OngoingSnapshot {
            filepath: PathBuf::from(format!(
                "{archive_name_without_extension}.{}",
                self.compression_algorithm.tar_file_extension()
            )),
            filesize: 0,
        };
        *value = Some(snapshot.clone());

        Ok(snapshot)
    }

    fn snapshot_subset(
        &self,
        archive_name_without_extension: &str,
        _files: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot> {
        self.snapshot_all(archive_name_without_extension)
    }
}

/// Snapshotter that writes empty files to the filesystem. Used for testing purposes.
pub struct FakeSnapshotter {
    work_dir: PathBuf,
    compression_algorithm: CompressionAlgorithm,
}

impl FakeSnapshotter {
    /// `FakeSnapshotter` factory, with a default compression algorithm of `Gzip`.
    pub fn new<T: AsRef<Path>>(work_dir: T) -> Self {
        Self {
            work_dir: work_dir.as_ref().to_path_buf(),
            compression_algorithm: CompressionAlgorithm::Gzip,
        }
    }

    /// Set the compression algorithm used to for the output file name extension.
    pub fn with_compression_algorithm(
        mut self,
        compression_algorithm: CompressionAlgorithm,
    ) -> Self {
        self.compression_algorithm = compression_algorithm;
        self
    }
}

impl Snapshotter for FakeSnapshotter {
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<OngoingSnapshot> {
        let fake_archive_path = self.work_dir.join(format!(
            "{archive_name_without_extension}.{}",
            self.compression_algorithm.tar_file_extension()
        ));
        if let Some(archive_dir) = fake_archive_path.parent() {
            fs::create_dir_all(archive_dir).unwrap();
        }
        File::create(&fake_archive_path).unwrap();

        Ok(OngoingSnapshot {
            filepath: fake_archive_path,
            filesize: 0,
        })
    }

    fn snapshot_subset(
        &self,
        archive_name_without_extension: &str,
        _files: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot> {
        self.snapshot_all(archive_name_without_extension)
    }
}

#[cfg(test)]
mod tests {
    use crate::services::snapshotter::test_tools::*;

    use super::*;

    mod dumb_snapshotter {
        use super::*;

        #[test]
        fn test_dumb_snapshotter_snapshot_return_archive_named_with_compression_algorithm_and_size_of_0(
        ) {
            let snapshotter = DumbSnapshotter::new(CompressionAlgorithm::Gzip);
            let snapshot = snapshotter.snapshot_all("archive").unwrap();

            assert_eq!(PathBuf::from("archive.tar.gz"), *snapshot.get_file_path());
            assert_eq!(0, *snapshot.get_file_size());

            let snapshot = snapshotter
                .snapshot_subset("archive", vec![PathBuf::from("whatever")])
                .unwrap();
            assert_eq!(PathBuf::from("archive.tar.gz"), *snapshot.get_file_path());
            assert_eq!(0, *snapshot.get_file_size());
        }

        #[test]
        fn test_dumb_snapshotter() {
            let snapshotter = DumbSnapshotter::new(CompressionAlgorithm::Zstandard);
            assert!(snapshotter
                .get_last_snapshot()
                .expect(
                    "Dumb snapshotter::get_last_snapshot should not fail when no last snapshot."
                )
                .is_none());

            let snapshot = snapshotter
                .snapshot_all("whatever")
                .expect("Dumb snapshotter::snapshot should not fail.");
            assert_eq!(
                Some(snapshot),
                snapshotter.get_last_snapshot().expect(
                    "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
                )
            );

            let snapshot = snapshotter
                .snapshot_subset("another_whatever", vec![PathBuf::from("subdir")])
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
            let test_dir = get_test_directory("fake_snapshotter_snapshot_all_create_empty_file_located_at_work_dir_joined_filepath");
            let fake_snapshotter = FakeSnapshotter::new(&test_dir)
                .with_compression_algorithm(CompressionAlgorithm::Gzip);

            for filename in [
                "direct_child",
                "one_level_subdir/child",
                "two_levels/subdir/child",
            ] {
                let snapshot = fake_snapshotter.snapshot_all(filename).unwrap();

                assert_eq!(
                    snapshot.get_file_path(),
                    &test_dir.join(filename).with_extension("tar.gz")
                );
                assert!(snapshot.get_file_path().is_file());
            }
        }

        #[test]
        fn snapshot_subset_create_empty_file_located_at_work_dir_joined_filepath() {
            let test_dir = get_test_directory("fake_snapshotter_snapshot_subset_create_empty_file_located_at_work_dir_joined_filepath");
            let fake_snapshotter = FakeSnapshotter::new(&test_dir)
                .with_compression_algorithm(CompressionAlgorithm::Zstandard);

            for filename in [
                "direct_child",
                "one_level_subdir/child",
                "two_levels/subdir/child",
            ] {
                let snapshot = fake_snapshotter.snapshot_subset(filename, vec![]).unwrap();

                assert_eq!(
                    snapshot.get_file_path(),
                    &test_dir.join(filename).with_extension("tar.zst")
                );
                assert!(snapshot.get_file_path().is_file());
            }
        }
    }
}
