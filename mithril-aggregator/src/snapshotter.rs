use anyhow::{anyhow, Context};
use flate2::Compression;
use flate2::{read::GzDecoder, write::GzEncoder};
use slog::{info, warn, Logger};
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::RwLock;
use tar::{Archive, Entry, EntryType};
use thiserror::Error;
use zstd::{Decoder, Encoder};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::dependency_injection::DependenciesBuilderError;
use crate::ZstandardCompressionParameters;

/// Define the ability to create snapshots.
pub trait Snapshotter: Sync + Send {
    /// Create a new snapshot with the given filepath.
    fn snapshot_all(&self, filepath: &Path) -> StdResult<OngoingSnapshot>;

    /// Create a new snapshot with the given filepath from a subset of directories and files.
    fn snapshot_subset(&self, filepath: &Path, files: Vec<PathBuf>) -> StdResult<OngoingSnapshot>;
}

/// Compression algorithm and parameters of the [CompressedArchiveSnapshotter].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SnapshotterCompressionAlgorithm {
    /// Gzip compression format
    Gzip,
    /// Zstandard compression format
    Zstandard(ZstandardCompressionParameters),
}

impl From<ZstandardCompressionParameters> for SnapshotterCompressionAlgorithm {
    fn from(params: ZstandardCompressionParameters) -> Self {
        Self::Zstandard(params)
    }
}

/// Define multiple ways to append content to a tar archive.
trait TarAppender {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()>;
}

struct AppenderDirAll {
    db_directory: PathBuf,
}

impl TarAppender for AppenderDirAll {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        tar.append_dir_all(".", &self.db_directory)
            .map_err(SnapshotError::CreateArchiveError)
            .with_context(|| {
                format!(
                    "Can not add directory: '{}' to the archive",
                    self.db_directory.display()
                )
            })?;
        Ok(())
    }
}

struct AppenderEntries {
    entries: Vec<PathBuf>,
    db_directory: PathBuf,
}

impl TarAppender for AppenderEntries {
    fn append<T: Write>(&self, tar: &mut tar::Builder<T>) -> StdResult<()> {
        for entry in &self.entries {
            let entry_path = self.db_directory.join(entry);
            if entry_path.is_dir() {
                tar.append_dir_all(entry, entry_path.clone())
                    .with_context(|| {
                        format!(
                            "Can not add directory: '{}' to the archive",
                            entry_path.display()
                        )
                    })?;
            } else if entry_path.is_file() {
                let mut file = File::open(entry_path.clone())?;
                tar.append_file(entry, &mut file).with_context(|| {
                    format!(
                        "Can not add file: '{}' to the archive",
                        entry_path.display()
                    )
                })?;
            } else {
                return Err(anyhow!(
                    "The entry: '{}' is not valid",
                    entry_path.display()
                ));
            }
        }
        Ok(())
    }
}
/// Compressed Archive Snapshotter create a compressed file.
pub struct CompressedArchiveSnapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store ongoing snapshot
    ongoing_snapshot_directory: PathBuf,

    /// Compression algorithm used for the archive
    compression_algorithm: SnapshotterCompressionAlgorithm,

    logger: Logger,
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

impl Snapshotter for CompressedArchiveSnapshotter {
    fn snapshot_all(&self, filepath: &Path) -> StdResult<OngoingSnapshot> {
        let appender = AppenderDirAll {
            db_directory: self.db_directory.clone(),
        };

        self.snapshot(filepath, appender)
    }

    fn snapshot_subset(
        &self,
        filepath: &Path,
        entries: Vec<PathBuf>,
    ) -> StdResult<OngoingSnapshot> {
        if entries.is_empty() {
            return Err(anyhow!("Can not create snapshot with empty entries"));
        }

        let appender = AppenderEntries {
            db_directory: self.db_directory.clone(),
            entries,
        };

        self.snapshot(filepath, appender)
    }
}

impl CompressedArchiveSnapshotter {
    /// Snapshotter factory
    pub fn new(
        db_directory: PathBuf,
        ongoing_snapshot_directory: PathBuf,
        compression_algorithm: SnapshotterCompressionAlgorithm,
        logger: Logger,
    ) -> StdResult<CompressedArchiveSnapshotter> {
        if ongoing_snapshot_directory.exists() {
            fs::remove_dir_all(&ongoing_snapshot_directory).with_context(|| {
                format!(
                    "Can not remove snapshotter directory: '{}'.",
                    ongoing_snapshot_directory.display()
                )
            })?;
        }

        fs::create_dir(&ongoing_snapshot_directory).map_err(|e| {
            DependenciesBuilderError::Initialization {
                message: format!(
                    "Can not create snapshotter directory: '{}'.",
                    ongoing_snapshot_directory.display()
                ),
                error: Some(e.into()),
            }
        })?;

        Ok(Self {
            db_directory,
            ongoing_snapshot_directory,
            compression_algorithm,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    fn snapshot<T: TarAppender>(&self, filepath: &Path, appender: T) -> StdResult<OngoingSnapshot> {
        let archive_path = self.ongoing_snapshot_directory.join(filepath);
        if let Some(archive_dir) = archive_path.parent() {
            fs::create_dir_all(archive_dir).with_context(|| {
                format!(
                    "Can not create archive directory: '{}'",
                    archive_dir.display()
                )
            })?;
        }
        let filesize = self.create_and_verify_archive(&archive_path, appender).inspect_err(|_err| {
            if archive_path.exists() {
                if let Err(remove_error) = fs::remove_file(&archive_path) {
                    warn!(
                        self.logger, " > Post snapshotter.snapshot failure, could not remove temporary archive";
                        "archive_path" => archive_path.display(),
                        "error" => remove_error
                    );
                }
            }
        }).with_context(|| format!("CompressedArchiveSnapshotter can not create and verify archive: '{}'", archive_path.display()))?;

        Ok(OngoingSnapshot {
            filepath: archive_path,
            filesize,
        })
    }

    fn get_file_size(filepath: &Path) -> StdResult<u64> {
        let res = fs::metadata(filepath)
            .map_err(|e| SnapshotError::GeneralError(e.to_string()))?
            .len();
        Ok(res)
    }

    fn create_archive<T: TarAppender>(&self, archive_path: &Path, appender: T) -> StdResult<u64> {
        info!(
            self.logger,
            "Compressing {} into {}",
            self.db_directory.display(),
            archive_path.display()
        );

        let tar_file = File::create(archive_path)
            .map_err(SnapshotError::CreateArchiveError)
            .with_context(|| {
                format!("Error while creating the archive with path: {archive_path:?}")
            })?;

        match self.compression_algorithm {
            SnapshotterCompressionAlgorithm::Gzip => {
                let enc = GzEncoder::new(tar_file, Compression::default());
                let mut tar = tar::Builder::new(enc);

                appender
                    .append(&mut tar)
                    .with_context(|| "GzEncoder Builder failed to append content")?;

                let mut gz = tar
                    .into_inner()
                    .map_err(SnapshotError::CreateArchiveError)
                    .with_context(|| "GzEncoder Builder can not write the archive")?;
                gz.try_finish()
                    .map_err(SnapshotError::CreateArchiveError)
                    .with_context(|| "GzEncoder can not finish the output stream after writing")?;
            }
            SnapshotterCompressionAlgorithm::Zstandard(params) => {
                let mut enc = Encoder::new(tar_file, params.level)?;
                enc.multithread(params.number_of_workers)
                    .map_err(SnapshotError::CreateArchiveError)?;
                let mut tar = tar::Builder::new(enc);

                appender
                    .append(&mut tar)
                    .with_context(|| "ZstandardEncoder Builder failed to append content")?;

                let zstd = tar
                    .into_inner()
                    .map_err(SnapshotError::CreateArchiveError)
                    .with_context(|| "ZstandardEncoder Builder can not write the archive")?;
                zstd.finish()
                    .map_err(SnapshotError::CreateArchiveError)
                    .with_context(|| {
                        "ZstandardEncoder can not finish the output stream after writing"
                    })?;
            }
        }

        let filesize = Self::get_file_size(archive_path).with_context(|| {
            format!(
                "CompressedArchiveSnapshotter can not get file size of archive with path: '{}'",
                archive_path.display()
            )
        })?;

        Ok(filesize)
    }

    fn create_and_verify_archive<T: TarAppender>(
        &self,
        archive_path: &Path,
        appender: T,
    ) -> StdResult<u64> {
        let filesize = self
            .create_archive(archive_path, appender)
            .with_context(|| {
                format!(
                    "CompressedArchiveSnapshotter can not create archive with path: '{}''",
                    archive_path.display()
                )
            })?;
        self.verify_archive(archive_path).with_context(|| {
            format!(
                "CompressedArchiveSnapshotter can not verify archive with path: '{}''",
                archive_path.display()
            )
        })?;

        Ok(filesize)
    }

    // Verify if an archive is corrupted (i.e. at least one entry is invalid)
    fn verify_archive(&self, archive_path: &Path) -> StdResult<()> {
        info!(self.logger, "Verifying archive: {}", archive_path.display());

        let mut snapshot_file_tar = File::open(archive_path)
            .map_err(|e| SnapshotError::InvalidArchiveError(e.to_string()))?;
        snapshot_file_tar.seek(SeekFrom::Start(0))?;

        let mut snapshot_archive: Archive<Box<dyn Read>> = match self.compression_algorithm {
            SnapshotterCompressionAlgorithm::Gzip => {
                let snapshot_file_tar = GzDecoder::new(snapshot_file_tar);
                Archive::new(Box::new(snapshot_file_tar))
            }
            SnapshotterCompressionAlgorithm::Zstandard(_) => {
                let snapshot_file_tar = Decoder::new(snapshot_file_tar)?;
                Archive::new(Box::new(snapshot_file_tar))
            }
        };

        let unpack_temp_dir = std::env::temp_dir()
            .join("mithril_snapshotter_verify_archive")
            // Add the archive name to the directory to allow two verifications at the same time
            // (useful for tests).
            .join(
                archive_path
                    .file_name()
                    .ok_or(SnapshotError::VerifyArchiveError(format!(
                        "Could not append archive name to temp directory: archive `{}`",
                        archive_path.display(),
                    )))?,
            );

        fs::create_dir_all(&unpack_temp_dir).map_err(|e| {
            SnapshotError::VerifyArchiveError(format!(
                "Could not create directory `{}`: {e}",
                unpack_temp_dir.display(),
            ))
        })?;

        let unpack_temp_file = &unpack_temp_dir.join("unpack.tmp");

        let verify_result = {
            let mut result = Ok(());
            for e in snapshot_archive.entries()? {
                match e {
                    Err(e) => {
                        result = Err(anyhow!(SnapshotError::InvalidArchiveError(format!(
                            "invalid entry with error: '{:?}'",
                            e
                        ))));
                        break;
                    }
                    Ok(entry) => Self::unpack_and_delete_file_from_entry(entry, unpack_temp_file)?,
                };
            }
            result
        };

        // Always remove the temp directory
        fs::remove_dir_all(&unpack_temp_dir).map_err(|e| {
            SnapshotError::VerifyArchiveError(format!(
                "Could not remove directory `{}`: {e}",
                unpack_temp_dir.display(),
            ))
        })?;

        verify_result
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
                    )));
                }
                Ok(_) => {
                    if let Err(e) = fs::remove_file(unpack_file_path) {
                        return Err(SnapshotError::VerifyArchiveError(format!(
                            "can't remove temporary unpacked file with error: '{e:?}', file path: `{}`",
                            unpack_file_path.display()
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use uuid::Uuid;

    use mithril_common::{digesters::DummyCardanoDbBuilder, test_utils::TempDir};

    use crate::test_tools::TestLogger;

    use super::*;

    fn get_test_directory(dir_name: &str) -> PathBuf {
        TempDir::create("snapshotter", dir_name)
    }

    fn create_file(root: &Path, filename: &str) -> PathBuf {
        let file_path = PathBuf::from(filename);
        File::create(root.join(file_path.clone())).unwrap();
        file_path
    }

    fn create_dir(root: &Path, dirname: &str) -> PathBuf {
        let dir_path = PathBuf::from(dirname);
        std::fs::create_dir(root.join(dir_path.clone())).unwrap();
        dir_path
    }

    fn unpack_gz_decoder(test_dir: PathBuf, snapshot: OngoingSnapshot) -> PathBuf {
        let file_tar_gz = File::open(snapshot.get_file_path()).unwrap();
        let file_tar_gz_decoder = GzDecoder::new(file_tar_gz);
        let mut archive = Archive::new(file_tar_gz_decoder);
        let unpack_path = test_dir.join(create_dir(&test_dir, "unpack"));
        archive.unpack(&unpack_path).unwrap();

        unpack_path
    }

    // Generate unique name for the archive is mandatory to avoid conflicts during the verification.
    fn random_archive_name() -> String {
        format!("{}.tar.gz", Uuid::new_v4())
    }

    #[test]
    fn test_dumb_snapshotter_snasphot_return_archive_name_with_size_0() {
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
            .expect("Dumb snapshotter::get_last_snapshot should not fail when no last snapshot.")
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

    #[test]
    fn should_create_directory_if_does_not_exist() {
        let test_dir = get_test_directory("should_create_directory_if_does_not_exist");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("whatever");

        Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        assert!(pending_snapshot_directory.is_dir());
    }

    #[test]
    fn should_clean_pending_snapshot_directory_if_already_exists() {
        let test_dir =
            get_test_directory("should_clean_pending_snapshot_directory_if_already_exists");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("whatever");

        fs::create_dir_all(&pending_snapshot_directory).unwrap();

        File::create(pending_snapshot_directory.join("whatever.txt")).unwrap();

        Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        assert_eq!(0, fs::read_dir(pending_snapshot_directory).unwrap().count());
    }

    #[test]
    fn should_delete_tmp_file_in_pending_snapshot_directory_if_snapshotting_fail() {
        let test_dir = get_test_directory(
            "should_delete_tmp_file_in_pending_snapshot_directory_if_snapshotting_fail",
        );
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let db_directory = test_dir.join("db");

        let snapshotter = Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory,
                pending_snapshot_directory.clone(),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        // this file should not be deleted by the archive creation
        File::create(pending_snapshot_directory.join("other-process.file")).unwrap();

        let _ = snapshotter
            .snapshot_all(Path::new("whatever.tar.gz"))
            .expect_err("Snapshotter::snapshot should fail if the db is empty.");
        let remaining_files: Vec<String> = fs::read_dir(&pending_snapshot_directory)
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

        DummyCardanoDbBuilder::new(db_directory.as_os_str().to_str().unwrap())
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();

        let snapshotter = Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                pending_snapshot_directory.clone(),
                SnapshotterCompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        let appender = AppenderDirAll { db_directory };
        snapshotter
            .create_archive(
                &pending_snapshot_directory.join(Path::new(pending_snapshot_archive_file)),
                appender,
            )
            .expect("create_archive should not fail");
        snapshotter
            .verify_archive(
                &pending_snapshot_directory.join(Path::new(pending_snapshot_archive_file)),
            )
            .expect("verify_archive should not fail");

        snapshotter
            .snapshot_all(Path::new(pending_snapshot_archive_file))
            .expect("Snapshotter::snapshot should not fail.");
    }

    #[test]
    fn should_create_a_valid_archive_with_zstandard_snapshotter() {
        let test_dir =
            get_test_directory("should_create_a_valid_archive_with_zstandard_snapshotter");
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let pending_snapshot_archive_file = "archive.tar.zst";
        let db_directory = test_dir.join("db");

        DummyCardanoDbBuilder::new(db_directory.as_os_str().to_str().unwrap())
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();

        let snapshotter = Arc::new(
            CompressedArchiveSnapshotter::new(
                db_directory.clone(),
                pending_snapshot_directory.clone(),
                ZstandardCompressionParameters::default().into(),
                TestLogger::stdout(),
            )
            .unwrap(),
        );

        let appender = AppenderDirAll { db_directory };
        snapshotter
            .create_archive(
                &pending_snapshot_directory.join(Path::new(pending_snapshot_archive_file)),
                appender,
            )
            .expect("create_archive should not fail");
        snapshotter
            .verify_archive(
                &pending_snapshot_directory.join(Path::new(pending_snapshot_archive_file)),
            )
            .expect("verify_archive should not fail");

        snapshotter
            .snapshot_all(Path::new(pending_snapshot_archive_file))
            .expect("Snapshotter::snapshot should not fail.");
    }

    #[test]
    fn snapshot_subset_should_create_archive_only_for_specified_directories_and_files() {
        let test_dir = get_test_directory("only_for_specified_directories_and_files");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "file_to_archive.txt");
        let directory_not_to_archive_path = create_dir(&source, "directory_not_to_archive");
        let file_not_to_archive_path = create_file(&source, "file_not_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let snapshot = snapshotter
            .snapshot_subset(
                Path::new(&random_archive_name()),
                vec![
                    directory_to_archive_path.clone(),
                    file_to_archive_path.clone(),
                ],
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(test_dir, snapshot);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
        assert!(!unpack_path.join(directory_not_to_archive_path).exists());
        assert!(!unpack_path.join(file_not_to_archive_path).exists());
    }

    #[test]
    fn snapshot_subset_return_error_when_file_or_directory_not_exist() {
        let test_dir = get_test_directory("file_or_directory_not_exist");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        snapshotter
            .snapshot_subset(
                Path::new(&random_archive_name()),
                vec![PathBuf::from("not_exist")],
            )
            .expect_err("snapshot_subset should return error when file or directory not exist");
    }

    #[test]
    fn snapshot_subset_return_error_when_empty_entries() {
        let test_dir = get_test_directory("empty_entries");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        snapshotter
            .snapshot_subset(Path::new(&random_archive_name()), vec![])
            .expect_err("snapshot_subset should return error when entries is empty");
    }

    #[test]
    fn snapshot_subset_with_duplicate_files_and_directories() {
        let test_dir = get_test_directory("with_duplicate_files_and_directories");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        let directory_to_archive_path = create_dir(&source, "directory_to_archive");
        let file_to_archive_path = create_file(&source, "directory_to_archive/file_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            source,
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let snapshot = snapshotter
            .snapshot_subset(
                Path::new(&random_archive_name()),
                vec![
                    directory_to_archive_path.clone(),
                    directory_to_archive_path.clone(),
                    file_to_archive_path.clone(),
                    file_to_archive_path.clone(),
                ],
            )
            .unwrap();

        let unpack_path = unpack_gz_decoder(test_dir, snapshot);

        assert!(unpack_path.join(directory_to_archive_path).is_dir());
        assert!(unpack_path.join(file_to_archive_path).is_file());
    }
}
