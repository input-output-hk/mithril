use flate2::write::GzEncoder;
use flate2::Compression;
use slog_scope::info;
use std::error::Error as StdError;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::RwLock;
use thiserror::Error;

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

    /// Set when the snappshotter fails at uploading the snapshot.
    #[error("Upload file error: `{0}`")]
    UploadFileError(String),

    /// General error.
    #[error("Snapshot General Error: `{0}`")]
    GeneralError(String),
}

impl Snapshotter for GzipSnapshotter {
    fn snapshot(&self, archive_name: &str) -> Result<OngoingSnapshot, SnapshotError> {
        let filepath = self.create_archive(archive_name)?;
        let filesize = std::fs::metadata(&filepath)
            .map_err(|e| SnapshotError::GeneralError(e.to_string()))?
            .len();

        Ok(OngoingSnapshot { filepath, filesize })
    }
}

impl GzipSnapshotter {
    /// Snapshotter factory
    pub fn new(db_directory: PathBuf, ongoing_snapshot_directory: PathBuf) -> Self {
        Self {
            db_directory,
            ongoing_snapshot_directory,
        }
    }

    fn create_archive(&self, archive_name: &str) -> Result<PathBuf, SnapshotError> {
        let path = self.ongoing_snapshot_directory.join(archive_name);
        info!(
            "compressing {} into {}",
            self.db_directory.display(),
            path.display()
        );

        let tar_gz = File::create(&path).map_err(SnapshotError::CreateArchiveError)?;
        let enc = GzEncoder::new(tar_gz, Compression::default());
        let mut tar = tar::Builder::new(enc);

        tar.append_dir_all(".", &self.db_directory)
            .map_err(SnapshotError::CreateArchiveError)?;

        let mut gz = tar
            .into_inner()
            .map_err(SnapshotError::CreateArchiveError)?;
        gz.try_finish().map_err(SnapshotError::CreateArchiveError)?;

        Ok(path)
    }
}

/// Snapshotter that does nothing. It is mainly used for test puroposes.
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
    pub fn get_last_snapshot(
        &self,
    ) -> Result<Option<OngoingSnapshot>, Box<dyn StdError + Sync + Send>> {
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
    use super::*;

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
}
