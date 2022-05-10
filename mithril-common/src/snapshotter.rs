use crate::entities::{ImmutableNumber, Snapshot};
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
use std::path::{Path, PathBuf};
use tokio::time::{sleep, Duration};
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;
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

    async fn snapshot(&self) -> Result<ImmutableNumber, SnapshotError> {
        let archive_name = "testnet.tar.gz";
        let immutables = ImmutableFile::list_in_dir(&*self.db_directory)
            .map_err(|e| SnapshotError { reason: e })?;
        let last_immutable = immutables.last().ok_or(SnapshotError {
            reason: format!(
                "At least two immutables chunk should exists in `{:?}/immutable/`",
                self.db_directory
            ),
        })?;

        info!(self.logger, "#immutables: {}", immutables.len());

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

        Ok(last_immutable.number)
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

    fn compute_hash(&self, entries: &[ImmutableFile]) -> [u8; 32] {
        let mut hasher = Sha256::new();
        let mut progress = Progress {
            index: 0,
            total: entries.len(),
        };

        for (ix, entry) in entries.iter().enumerate() {
            let mut file = File::open(&entry.path).unwrap();

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

#[derive(Debug)]
struct ImmutableFile {
    path: PathBuf,
    number: ImmutableNumber,
}

impl ImmutableFile {
    fn new(path: PathBuf) -> Result<Self, String> {
        let filename = path
            .file_stem()
            .ok_or(format!("Couldn't extract the file stem for '{:?}'", path))?;
        let filename = filename.to_str().ok_or(format!(
            "Couldn't extract the filename as string for '{:?}'",
            path
        ))?;
        let immutable_number = filename
            .parse::<ImmutableNumber>()
            .map_err(|e| e.to_string())?;

        Ok(Self {
            path,
            number: immutable_number,
        })
    }

    /// List all [`ImmutableFile`] in a given directory.
    ///
    /// Important Note: It will skip the last chunk / primary / secondary trio since they're not yet
    /// complete.
    fn list_in_dir(dir: &str) -> Result<Vec<ImmutableFile>, String> {
        let mut files: Vec<ImmutableFile> = vec![];

        for path in WalkDir::new(dir)
            .into_iter()
            .filter_map(|file| file.ok())
            .map(|f| f.path().to_owned())
        {
            let metadata = path.metadata().map_err(|e| e.to_string())?;
            if metadata.is_file() && is_immutable(&path) {
                let immutable_file = ImmutableFile::new(path)?;
                files.push(immutable_file);
            }
        }
        files.sort_by(|left, right| left.number.cmp(&right.number));

        Ok(files.into_iter().rev().skip(3).rev().collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    const ROOT_FOLDER: &str = "tests-work-folder/";

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        let parent_dir = format!("{}{}", ROOT_FOLDER, subdir_name);
        let parent_dir = Path::new(&parent_dir).to_path_buf();

        if parent_dir.exists() {
            fs::remove_dir_all(&parent_dir)
                .expect(&*format!("Could not remove dir {:?}", parent_dir));
        }
        fs::create_dir_all(&parent_dir).expect(&*format!("Could not create dir {:?}", parent_dir));

        parent_dir
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(&file).unwrap();
            write!(source_file, "This is a test file named '{}'", filename).unwrap();
        }
    }

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

    #[test]
    fn list_immutable_file_should_skip_last_number() {
        let target_dir = get_test_dir("list_immutable_file_should_skip_last_number/immutable");
        let entries = vec![
            "123.chunk",
            "123.primary",
            "123.secondary",
            "125.chunk",
            "125.primary",
            "125.secondary",
            "0124.chunk",
            "0124.primary",
            "0124.secondary",
            "223.chunk",
            "223.primary",
            "223.secondary",
            "0423.chunk",
            "0423.primary",
            "0423.secondary",
            "0424.chunk",
            "0424.primary",
            "0424.secondary",
            "21.chunk",
            "21.primary",
            "21.secondary",
        ];
        create_fake_files(&target_dir, &entries);
        let result = ImmutableFile::list_in_dir(target_dir.parent().unwrap().to_str().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");

        assert_eq!(result.last().unwrap().number, 423);
        assert_eq!(
            result.len(),
            entries.len() - 3,
            "Expected to find {} files since The last (chunk, primary, secondary) trio is skipped, but found {}",
            entries.len() - 3,
            result.len(),
        );
    }

    #[test]
    fn list_immutable_file_should_works_in_a_empty_folder() {
        let target_dir =
            get_test_dir("list_immutable_file_should_works_even_in_a_empty_folder/immutable");
        let entries = vec![];
        create_fake_files(&target_dir, &entries);
        let result = ImmutableFile::list_in_dir(target_dir.parent().unwrap().to_str().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");

        assert!(result.is_empty());
    }
}
