use flate2::Compression;
use flate2::{read::GzDecoder, write::GzEncoder};
use mithril_common::StdResult;
use slog_scope::{info, warn};
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::sync::RwLock;
use tar::{Archive, Entry, EntryType};
use thiserror::Error;

use crate::dependency_injection::DependenciesBuilderError;

/// Define the ability to create snapshots.
pub trait Snapshotter: Sync + Send {
    /// Create a new snapshot with the given archive name.
    fn snapshot(&self, archive_name: &str) -> Result<OngoingSnapshot, SnapshotError>;
}

/// Gzip Snapshotter create a compressed file.
pub struct GzipSnapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store ongoing snapshot
    ongoing_snapshot_directory: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OngoingSnapshot {
    filepath: PathBuf,
    filesize: u64,
}

impl OngoingSnapshot {
    pub fn new(filepath: PathBuf, filesize: u64) -> Self {
        Self { filepath, filesize }
    }

    pub fn get_file_path(&self) -> &PathBuf {
        &self.filepath
    }

    pub fn get_file_size(&self) -> &u64 {
        &self.filesize
    }
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

impl Snapshotter for GzipSnapshotter {
    fn snapshot(&self, archive_name: &str) -> Result<OngoingSnapshot, SnapshotError> {
        let archive_path = self.ongoing_snapshot_directory.join(archive_name);
        let filesize = self.create_and_verify_archive(&archive_path).map_err(|err| {
            if archive_path.exists() {
                if let Err(remove_error) = std::fs::remove_file(&archive_path) {
                    warn!(
                        " > Post snapshotter.snapshot failure, could not remove temporary archive at path: path:{}, err: {}",
                        archive_path.display(),
                        remove_error
                    );
                }
            }

            err
        })?;

        Ok(OngoingSnapshot {
            filepath: archive_path,
            filesize,
        })
    }
}

impl GzipSnapshotter {
    /// Snapshotter factory
    pub fn new(
        db_directory: PathBuf,
        ongoing_snapshot_directory: PathBuf,
    ) -> StdResult<GzipSnapshotter> {
        if ongoing_snapshot_directory.exists() {
            std::fs::remove_dir_all(&ongoing_snapshot_directory)?;
        }

        std::fs::create_dir(&ongoing_snapshot_directory).map_err(|e| {
            DependenciesBuilderError::Initialization {
                message: format!(
                    "Cannot create snapshotter directory '{}'.",
                    ongoing_snapshot_directory.display()
                ),
                error: Some(e.into()),
            }
        })?;

        Ok(Self {
            db_directory,
            ongoing_snapshot_directory,
        })
    }

    fn get_file_size(filepath: &Path) -> Result<u64, SnapshotError> {
        let res = std::fs::metadata(filepath)
            .map_err(|e| SnapshotError::GeneralError(e.to_string()))?
            .len();
        Ok(res)
    }

    fn create_archive(&self, archive_path: &Path) -> Result<u64, SnapshotError> {
        info!(
            "compressing {} into {}",
            self.db_directory.display(),
            archive_path.display()
        );

        let tar_gz = File::create(archive_path).map_err(SnapshotError::CreateArchiveError)?;
        let enc = GzEncoder::new(tar_gz, Compression::default());
        let mut tar = tar::Builder::new(enc);

        tar.append_dir_all(".", &self.db_directory)
            .map_err(SnapshotError::CreateArchiveError)?;

        let mut gz = tar
            .into_inner()
            .map_err(SnapshotError::CreateArchiveError)?;
        gz.try_finish().map_err(SnapshotError::CreateArchiveError)?;

        let filesize = Self::get_file_size(archive_path)?;

        Ok(filesize)
    }

    fn create_and_verify_archive(&self, archive_path: &Path) -> Result<u64, SnapshotError> {
        let filesize = self.create_archive(archive_path)?;
        self.verify_archive(archive_path)?;

        Ok(filesize)
    }

    // Verify if an archive is corrupted (i.e. at least one entry is invalid)
    fn verify_archive(&self, archive_path: &Path) -> Result<(), SnapshotError> {
        info!("verifying archive: {}", archive_path.display());

        let mut snapshot_file_tar_gz = File::open(archive_path)
            .map_err(|e| SnapshotError::InvalidArchiveError(e.to_string()))?;

        snapshot_file_tar_gz.seek(SeekFrom::Start(0))?;
        let snapshot_file_tar = GzDecoder::new(snapshot_file_tar_gz);
        let mut snapshot_archive = Archive::new(snapshot_file_tar);

        let unpack_temp_dir = std::env::temp_dir().join("mithril_snapshotter_verify_archive");
        if unpack_temp_dir.exists() {
            fs::remove_dir_all(&unpack_temp_dir)
                .map_err(|e| SnapshotError::VerifyArchiveError(e.to_string()))?;
        }
        fs::create_dir_all(&unpack_temp_dir)
            .map_err(|e| SnapshotError::VerifyArchiveError(e.to_string()))?;
        let unpack_temp_file = &unpack_temp_dir.join("unpack.tmp");

        for e in snapshot_archive.entries()? {
            match e {
                Err(e) => {
                    return Err(SnapshotError::InvalidArchiveError(format!(
                        "invalid entry with error: '{:?}'",
                        e
                    )))
                }
                Ok(entry) => Self::unpack_and_delete_file_from_entry(entry, unpack_temp_file)?,
            }
        }

        Ok(())
    }

    // Helper to unpack and delete a file from en entry, for archive verification purpose
    fn unpack_and_delete_file_from_entry<R: Read>(
        entry: Entry<R>,
        unpack_file_path: &Path,
    ) -> Result<(), SnapshotError> {
        if entry.header().entry_type() != EntryType::Directory {
            let mut file = entry;
            match file.unpack(unpack_file_path) {
                Err(e) => {
                    return Err(SnapshotError::InvalidArchiveError(format!(
                        "can't unpack entry with error: '{:?}'",
                        e
                    )))
                }
                Ok(_) => {
                    if let Err(e) = fs::remove_file(unpack_file_path) {
                        return Err(SnapshotError::VerifyArchiveError(format!(
                            "can't remove temporary unpacked file with error: '{:?}'",
                            e
                        )));
                    }
                }
            }
        }

        Ok(())
    }
}

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
    fn snapshot(&self, archive_name: &str) -> Result<OngoingSnapshot, SnapshotError> {
        let mut value = self
            .last_snapshot
            .write()
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?;
        let snapshot = OngoingSnapshot {
            filepath: Path::new(archive_name).to_path_buf(),
            filesize: 0,
        };
        *value = Some(snapshot.clone());

        Ok(snapshot)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use mithril_common::digesters::DummyImmutablesDbBuilder;

    use super::*;

    fn get_test_directory(dir_name: &str) -> PathBuf {
        let test_dir = std::env::temp_dir()
            .join("mithril_test")
            .join("snapshotter")
            .join(dir_name);
        if test_dir.exists() {
            std::fs::remove_dir_all(&test_dir).unwrap();
        }
        std::fs::create_dir_all(&test_dir).unwrap();

        test_dir
    }

    #[test]
    fn test_dumb_snapshotter() {
        let snapshotter = DumbSnapshotter::new();
        assert!(snapshotter
            .get_last_snapshot()
            .expect("Dumb snapshotter::get_last_snapshot should not fail when no last snapshot.")
            .is_none());

        let snapshot = snapshotter
            .snapshot("whatever")
            .expect("Dumb snapshotter::snapshot should not fail.");
        assert_eq!(
            Some(snapshot),
            snapshotter.get_last_snapshot().expect(
                "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
            )
        );
    }

    #[test]
    fn should_create_directory_if_does_not_exist() {
        let test_dir = get_test_directory("should_create_directory_if_does_not_exist");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = std::env::temp_dir().join("whatever");

        Arc::new(GzipSnapshotter::new(db_directory, pending_snapshot_directory.clone()).unwrap());

        assert!(pending_snapshot_directory.is_dir());
    }

    #[test]
    fn should_clean_pending_snapshot_directory_if_already_exists() {
        let test_dir =
            get_test_directory("should_clean_pending_snapshot_directory_if_already_exists");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = std::env::temp_dir().join("whatever");

        std::fs::create_dir_all(&pending_snapshot_directory).unwrap();

        File::create(pending_snapshot_directory.join("whatever.txt")).unwrap();

        Arc::new(GzipSnapshotter::new(db_directory, pending_snapshot_directory.clone()).unwrap());

        assert_eq!(
            0,
            std::fs::read_dir(pending_snapshot_directory)
                .unwrap()
                .count()
        );
    }

    #[test]
    fn should_delete_tmp_file_in_pending_snapshot_directory_if_snapshotting_fail() {
        let test_dir = get_test_directory(
            "should_delete_tmp_file_in_pending_snapshot_directory_if_snapshotting_fail",
        );
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("db");

        let snapshotter = Arc::new(
            GzipSnapshotter::new(db_directory, pending_snapshot_directory.clone()).unwrap(),
        );

        // this file should not be deleted by the archive creation
        File::create(pending_snapshot_directory.join("other-process.file")).unwrap();

        let _ = snapshotter
            .snapshot("whatever.tar.gz")
            .expect_err("Snapshotter::snapshot should fail if the db is empty.");
        let remaining_files: Vec<String> = std::fs::read_dir(&pending_snapshot_directory)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect();

        assert_eq!(vec!["other-process.file".to_string()], remaining_files);
    }

    #[test]
    fn should_create_a_valid_archive_with_gzip_snapshotter() {
        let test_dir = get_test_directory("should_create_a_valid_archive_with_gzip_snapshotter");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let pending_snapshot_archive_file = "archive.tar.gz";
        let db_directory = test_dir.join("db");

        DummyImmutablesDbBuilder::new(db_directory.as_os_str().to_str().unwrap())
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();

        let snapshotter = Arc::new(
            GzipSnapshotter::new(db_directory, pending_snapshot_directory.clone()).unwrap(),
        );

        snapshotter
            .create_archive(
                &pending_snapshot_directory.join(Path::new(pending_snapshot_archive_file)),
            )
            .expect("create_archive should not fail");
        snapshotter
            .verify_archive(
                &pending_snapshot_directory.join(Path::new(pending_snapshot_archive_file)),
            )
            .expect("verify_archive should not fail");

        snapshotter
            .snapshot(pending_snapshot_archive_file)
            .expect("Snapshotter::snapshot should not fail.");
    }
}
