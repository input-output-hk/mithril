use flate2::write::GzEncoder;
use flate2::Compression;
use slog_scope::info;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use thiserror::Error;

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

impl Snapshotter {
    /// Snapshotter factory
    pub fn new(db_directory: PathBuf, snapshot_directory: PathBuf) -> Self {
        Self {
            db_directory,
            snapshot_directory,
        }
    }

    pub fn snapshot(&self, archive_name: &str) -> Result<PathBuf, SnapshotError> {
        self.create_archive(archive_name)
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

#[cfg(test)]
mod tests {}
