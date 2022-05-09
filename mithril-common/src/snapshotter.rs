use crate::entities::Snapshot;
use chrono::prelude::*;
use cloud_storage::bucket::Entity;
use cloud_storage::bucket_access_control::Role;
use cloud_storage::object_access_control::NewObjectAccessControl;
use cloud_storage::Client;
use flate2::write::GzEncoder;
use flate2::Compression;

use sha2::{Digest, Sha256};
use slog::{error, info, Logger};
use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::SeekFrom;
use std::path::Path;
use tokio::time::{sleep, Duration};
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

use walkdir::DirEntry;
use walkdir::WalkDir;

/// Snapshotter
pub struct Snapshotter {
    /// Interval between each snapshot, in seconds
    interval: u32,

    /// DB directory to snapshot
    db_directory: String,

    /// The logger where the logs should be written
    logger: Logger,
}

#[derive(Debug)]
struct SnapshotError {
    /// Detailed error
    #[allow(dead_code)] // Indirectly used in Snapshotter::run when logging the error
    reason: String,
}

impl From<io::Error> for SnapshotError {
    fn from(err: io::Error) -> Self {
        SnapshotError {
            reason: err.to_string(),
        }
    }
}

impl Snapshotter {
    /// Server factory
    pub fn new(interval: u32, db_directory: String, logger: Logger) -> Self {
        Self {
            interval,
            db_directory,
            logger,
        }
    }

    /// Start
    pub async fn run(&self) {
        info!(self.logger, "Starting Snapshotter");
        loop {
            info!(self.logger, "Snapshotting");
            if let Err(e) = self.snapshot().await {
                error!(self.logger, "{:?}", e)
            }
            info!(self.logger, "Sleeping for {}", self.interval);
            sleep(Duration::from_millis(self.interval.into())).await;
        }
    }

    async fn snapshot(&self) -> Result<(), SnapshotError> {
        let archive_name = "testnet.tar.gz";
        let files = list_files(&*self.db_directory);
        let immutables: Vec<&DirEntry> = files
            .iter()
            .filter(|entry| is_immutable(entry.path()))
            .collect();

        info!(
            self.logger,
            "#files: {}, #immutables: {}",
            files.len(),
            immutables.len()
        );

        let hash = self.compute_hash(&immutables);
        let digest = hex::encode(hash);
        info!(self.logger, "snapshot hash: {}", digest);

        let size = self.create_archive(archive_name)?;

        let timestamp: DateTime<Utc> = Utc::now();
        let created_at = format!("{:?}", timestamp);

        let snapshots = vec![Snapshot {
            digest,
            certificate_hash: "".to_string(),
            size,
            created_at,
            locations: vec![format!(
                "https://storage.googleapis.com/cardano-testnet/{}",
                archive_name
            )],
        }];

        info!(
            self.logger,
            "snapshot: {}",
            serde_json::to_string(&snapshots).unwrap()
        );
        serde_json::to_writer(&File::create("snapshots.json").unwrap(), &snapshots).unwrap();

        self.upload_file(archive_name).await?;
        self.upload_file("snapshots.json").await?;

        Ok(())
    }

    fn create_archive(&self, archive_name: &str) -> Result<u64, SnapshotError> {
        let path = Path::new(".").join(archive_name);
        let tar_gz = match File::create(&path) {
            Err(e) => {
                return Err(SnapshotError {
                    reason: format!("cannot create archive {}: {}", &path.to_str().unwrap(), e),
                })
            }
            Ok(f) => f,
        };
        let enc = GzEncoder::new(tar_gz, Compression::default());
        let mut tar = tar::Builder::new(enc);

        info!(
            self.logger,
            "compressing {} into {}",
            &self.db_directory,
            &path.to_str().unwrap()
        );

        tar.append_dir_all(".", &self.db_directory)?;

        // complete gz encoding and retrieve underlying file to compute size accurately
        // TODO: proper error handling, like everywhere else...
        let mut gz = tar.into_inner()?;
        gz.try_finish()?;
        let mut f = gz.finish()?;
        let size: u64 = f.seek(SeekFrom::End(0))?;

        Ok(size)
    }

    async fn upload_file(&self, filename: &str) -> Result<(), SnapshotError> {
        if env::var("GOOGLE_APPLICATION_CREDENTIALS_JSON").is_err() {
            return Err(SnapshotError {
                reason: "Missing GOOGLE_APPLICATION_CREDENTIALS_JSON environment variable"
                    .to_string(),
            });
        };

        info!(self.logger, "uploading {}", filename);
        let client = Client::default();
        let file = tokio::fs::File::open(filename).await.unwrap();
        let stream = FramedRead::new(file, BytesCodec::new());
        let response = client
            .object()
            .create_streamed(
                "cardano-testnet",
                stream,
                None,
                filename,
                "application/octet-stream",
            )
            .await;

        if let Err(e) = response {
            return Err(SnapshotError {
                reason: e.to_string(),
            });
        };

        info!(self.logger, "uploaded {}", filename);

        // ensure the uploaded file as public read access
        // when a file is uploaded to gcloud storage its permissions are overwritten so
        // we need to put them back
        let new_bucket_access_control = NewObjectAccessControl {
            entity: Entity::AllUsers,
            role: Role::Reader,
        };

        info!(
            self.logger,
            "updating acl for {}: {:?}", filename, new_bucket_access_control
        );

        let acl = client
            .object_access_control()
            .create("cardano-testnet", filename, &new_bucket_access_control)
            .await;

        if let Err(e) = acl {
            return Err(SnapshotError {
                reason: e.to_string(),
            });
        };

        info!(self.logger, "updated acl for {} ", filename);

        Ok(())
    }

    fn compute_hash(&self, entries: &[&DirEntry]) -> [u8; 32] {
        let mut hasher = Sha256::new();
        let mut progress = Progress {
            index: 0,
            total: entries.len(),
        };

        for (ix, &entry) in entries.iter().enumerate() {
            let mut file = File::open(entry.path()).unwrap();

            io::copy(&mut file, &mut hasher).unwrap();

            if progress.report(ix) {
                info!(self.logger, "hashing: {}", &progress);
            }
        }

        hasher.finalize().into()
    }
}

struct Progress {
    index: usize,
    total: usize,
}

impl Progress {
    fn report(&mut self, ix: usize) -> bool {
        self.index = ix;
        ix % (self.total / 20) == 0
    }

    fn percent(&self) -> f64 {
        (self.index as f64 * 100.0 / self.total as f64).ceil()
    }
}

impl std::fmt::Display for Progress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}/{} ({}%)", self.index, self.total, self.percent())
    }
}

fn is_immutable(path: &Path) -> bool {
    let immutable = OsStr::new("immutable");
    path.iter().any(|component| component == immutable)
}

fn list_files(dir: &str) -> Vec<DirEntry> {
    let mut files: Vec<DirEntry> = vec![];

    for file in WalkDir::new(dir).into_iter().filter_map(|file| file.ok()) {
        if file.metadata().unwrap().is_file() {
            files.push(file);
        }
    }
    files
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reports_progress_every_5_percent() {
        let mut progress = Progress {
            index: 0,
            total: 7000,
        };

        assert!(!progress.report(1));
        assert!(!progress.report(4));
        assert!(progress.report(350));
        assert!(!progress.report(351));
    }
}
