use flate2::write::GzEncoder;
use flate2::Compression;
use slog_scope::info;
use std::error::Error as StdError;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::sync::RwLock;
use thiserror::Error;

pub trait SnapshotterTrait: Sync + Send {
    fn snapshot(&self, archive_name: &str) -> Result<PathBuf, SnapshotError>;
}

/// Snapshotter
pub struct Snapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store snapshot
    snapshot_directory: PathBuf,
}

#[derive(Error, Debug)]
pub enum SnapshotError {
    #[error("Create archive error: ")]
    CreateArchiveError(#[from] io::Error),

    #[error("Upload file error: `{0}`")]
    UploadFileError(String),
}

impl SnapshotterTrait for Snapshotter {
    fn snapshot(&self, archive_name: &str) -> Result<PathBuf, SnapshotError> {
        self.create_archive(archive_name)
    }
}

impl Snapshotter {
    /// Snapshotter factory
    pub fn new(db_directory: PathBuf, snapshot_directory: PathBuf) -> Self {
        Self {
            db_directory,
            snapshot_directory,
        }
    }

    fn create_archive(&self, archive_name: &str) -> Result<PathBuf, SnapshotError> {
        let path = self.snapshot_directory.join(archive_name);
        let tar_gz = File::create(&path).map_err(SnapshotError::CreateArchiveError)?;
        let enc = GzEncoder::new(tar_gz, Compression::default());
        let mut tar = tar::Builder::new(enc);

        info!(
            "compressing {} into {}",
            self.db_directory.display(),
            path.display()
        );

        tar.append_dir_all(".", &self.db_directory)
            .map_err(SnapshotError::CreateArchiveError)?;

        let mut gz = tar
            .into_inner()
            .map_err(SnapshotError::CreateArchiveError)?;
        gz.try_finish().map_err(SnapshotError::CreateArchiveError)?;

        Ok(path)
    }
}

struct DumbSnapshotter {
    last_snapshot: RwLock<Option<String>>,
}

impl DumbSnapshotter {
    pub fn new() -> Self {
        Self {
            last_snapshot: RwLock::new(None),
        }
    }

    pub fn get_last_snapshot(&self) -> Result<Option<String>, Box<dyn StdError + Sync + Send>> {
        let value = self
            .last_snapshot
            .read()
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?
            .as_ref()
            .map(|v| v.clone());

        Ok(value)
    }
}

impl SnapshotterTrait for DumbSnapshotter {
    fn snapshot(&self, archive_name: &str) -> Result<PathBuf, SnapshotError> {
        let mut value = self
            .last_snapshot
            .write()
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?;
        *value = Some(archive_name.to_string());

        Ok(PathBuf::new())
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

        let _res = snapshotter
            .snapshot("whatever")
            .expect("Dumb snapshotter::snapshot should not fail.");
        assert_eq!(
            Some("whatever".to_string()),
            snapshotter.get_last_snapshot().expect(
                "Dumb snapshotter::get_last_snapshot should not fail when some last snapshot."
            )
        );
    }
}
