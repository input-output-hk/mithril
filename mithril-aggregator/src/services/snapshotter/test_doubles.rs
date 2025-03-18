use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::RwLock;

use mithril_common::entities::{CompressionAlgorithm, ImmutableFileNumber};
use mithril_common::StdResult;

use crate::services::Snapshotter;
use crate::tools::file_archiver::FileArchive;

/// Snapshotter that does nothing. It is mainly used for test purposes.
pub struct DumbSnapshotter {
    last_snapshot: RwLock<Option<FileArchive>>,
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
    pub fn get_last_snapshot(&self) -> StdResult<Option<FileArchive>> {
        let value = self.last_snapshot.read().unwrap().as_ref().cloned();

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
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<FileArchive> {
        let mut value = self.last_snapshot.write().unwrap();
        let snapshot = FileArchive::new(
            PathBuf::from(format!(
                "{archive_name_without_extension}.{}",
                self.compression_algorithm.tar_file_extension()
            )),
            0,
            0,
            self.compression_algorithm,
        );
        *value = Some(snapshot.clone());

        Ok(snapshot)
    }

    fn snapshot_all_completed_immutables(
        &self,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        self.snapshot_all(archive_name_without_extension)
    }

    fn snapshot_ancillary(
        &self,
        _immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        self.snapshot_all(archive_name_without_extension)
    }

    fn snapshot_immutable_trio(
        &self,
        _immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        self.snapshot_all(archive_name_without_extension)
    }

    fn compression_algorithm(&self) -> CompressionAlgorithm {
        self.compression_algorithm
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
    fn snapshot_all(&self, archive_name_without_extension: &str) -> StdResult<FileArchive> {
        let fake_archive_path = self.work_dir.join(format!(
            "{archive_name_without_extension}.{}",
            self.compression_algorithm.tar_file_extension()
        ));
        if let Some(archive_dir) = fake_archive_path.parent() {
            fs::create_dir_all(archive_dir).unwrap();
        }
        File::create(&fake_archive_path).unwrap();

        Ok(FileArchive::new(
            fake_archive_path,
            0,
            0,
            self.compression_algorithm,
        ))
    }

    fn snapshot_all_completed_immutables(
        &self,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        self.snapshot_all(archive_name_without_extension)
    }

    fn snapshot_ancillary(
        &self,
        _immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        self.snapshot_all(archive_name_without_extension)
    }

    fn snapshot_immutable_trio(
        &self,
        _immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        self.snapshot_all(archive_name_without_extension)
    }

    fn compression_algorithm(&self) -> CompressionAlgorithm {
        self.compression_algorithm
    }
}

#[cfg(test)]
mod tests {
    use crate::services::snapshotter::test_tools::*;

    use super::*;

    mod dumb_snapshotter {
        use super::*;

        #[test]
        fn return_parametrized_compression_algorithm() {
            let snapshotter = DumbSnapshotter::new(CompressionAlgorithm::Zstandard);
            assert_eq!(
                CompressionAlgorithm::Zstandard,
                snapshotter.compression_algorithm()
            );
        }

        #[test]
        fn test_dumb_snapshotter_snapshot_return_archive_named_with_compression_algorithm_and_size_of_0(
        ) {
            let snapshotter = DumbSnapshotter::new(CompressionAlgorithm::Gzip);
            let snapshot = snapshotter.snapshot_all("archive").unwrap();

            assert_eq!(PathBuf::from("archive.tar.gz"), *snapshot.get_file_path());
            assert_eq!(0, snapshot.get_archive_size());

            let snapshot = snapshotter
                .snapshot_all_completed_immutables("archive_full_immutables")
                .unwrap();
            assert_eq!(
                PathBuf::from("archive_full_immutables.tar.gz"),
                *snapshot.get_file_path()
            );
            assert_eq!(0, snapshot.get_archive_size());

            let snapshot = snapshotter
                .snapshot_ancillary(3, "archive_ancillary")
                .unwrap();
            assert_eq!(
                PathBuf::from("archive_ancillary.tar.gz"),
                *snapshot.get_file_path()
            );
            assert_eq!(0, snapshot.get_archive_size());

            let snapshot = snapshotter
                .snapshot_immutable_trio(4, "archive_immutable_trio")
                .unwrap();
            assert_eq!(
                PathBuf::from("archive_immutable_trio.tar.gz"),
                *snapshot.get_file_path()
            );
            assert_eq!(0, snapshot.get_archive_size());
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

            {
                let full_snapshot = snapshotter
                    .snapshot_all("whatever")
                    .expect("Dumb snapshotter::snapshot_all should not fail.");
                assert_eq!(
                    Some(full_snapshot),
                    snapshotter.get_last_snapshot().expect(
                        "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
                    )
                );
            }
            {
                let full_immutables_snapshot = snapshotter
                    .snapshot_all_completed_immutables("whatever")
                    .expect("Dumb snapshotter::snapshot_all_completed_immutables should not fail.");
                assert_eq!(
                    Some(full_immutables_snapshot),
                    snapshotter.get_last_snapshot().expect(
                        "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
                    )
                );
            }
            {
                let ancillary_snapshot = snapshotter
                    .snapshot_ancillary(3, "whatever")
                    .expect("Dumb snapshotter::snapshot_ancillary should not fail.");
                assert_eq!(
                    Some(ancillary_snapshot),
                    snapshotter.get_last_snapshot().expect(
                        "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
                    )
                );
            }
            {
                let immutable_snapshot = snapshotter
                    .snapshot_immutable_trio(4, "whatever")
                    .expect("Dumb snapshotter::snapshot_immutable_trio should not fail.");
                assert_eq!(
                    Some(immutable_snapshot),
                    snapshotter.get_last_snapshot().expect(
                        "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
                    )
                );
            }
        }
    }

    mod fake_snapshotter {
        use super::*;

        #[test]
        fn return_parametrized_compression_algorithm() {
            let snapshotter = FakeSnapshotter::new("whatever")
                .with_compression_algorithm(CompressionAlgorithm::Zstandard);
            assert_eq!(
                CompressionAlgorithm::Zstandard,
                snapshotter.compression_algorithm()
            );
        }

        #[test]
        fn test_fake_snasphotter() {
            let test_dir = get_test_directory("test_fake_snasphotter");
            let fake_snapshotter = FakeSnapshotter::new(&test_dir)
                .with_compression_algorithm(CompressionAlgorithm::Gzip);

            for filename in [
                "direct_child",
                "one_level_subdir/child",
                "two_levels/subdir/child",
            ] {
                {
                    let full_snapshot = fake_snapshotter.snapshot_all(filename).unwrap();

                    assert_eq!(
                        full_snapshot.get_file_path(),
                        &test_dir.join(filename).with_extension("tar.gz")
                    );
                    assert!(full_snapshot.get_file_path().is_file());
                }
                {
                    let full_immutables_snapshot = fake_snapshotter
                        .snapshot_all_completed_immutables(filename)
                        .unwrap();

                    assert_eq!(
                        full_immutables_snapshot.get_file_path(),
                        &test_dir.join(filename).with_extension("tar.gz")
                    );
                    assert!(full_immutables_snapshot.get_file_path().is_file());
                }
                {
                    let ancillary_snapshot =
                        fake_snapshotter.snapshot_ancillary(3, filename).unwrap();

                    assert_eq!(
                        ancillary_snapshot.get_file_path(),
                        &test_dir.join(filename).with_extension("tar.gz")
                    );
                    assert!(ancillary_snapshot.get_file_path().is_file());
                }
                {
                    let immutable_snapshot = fake_snapshotter
                        .snapshot_immutable_trio(5, filename)
                        .unwrap();

                    assert_eq!(
                        immutable_snapshot.get_file_path(),
                        &test_dir.join(filename).with_extension("tar.gz")
                    );
                    assert!(immutable_snapshot.get_file_path().is_file());
                }
            }
        }
    }
}
