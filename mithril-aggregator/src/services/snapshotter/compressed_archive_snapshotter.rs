use anyhow::{anyhow, Context};
use flate2::{read::GzDecoder, write::GzEncoder, Compression};
use slog::{info, warn, Logger};
use std::{
    fs,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::{Path, PathBuf},
};
use tar::{Archive, Entry, EntryType};
use zstd::{Decoder, Encoder};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use super::{
    appender::{AppenderDirAll, AppenderEntries, TarAppender},
    OngoingSnapshot, SnapshotError, Snapshotter, SnapshotterCompressionAlgorithm,
};
use crate::dependency_injection::DependenciesBuilderError;

/// Compressed Archive Snapshotter create a compressed file.
pub struct CompressedArchiveSnapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store ongoing snapshot
    ongoing_snapshot_directory: PathBuf,

    /// Compression algorithm used for the archive
    compression_algorithm: SnapshotterCompressionAlgorithm,

    // Temporary directory to store the unpacked archive for verification
    temp_dir: PathBuf,

    logger: Logger,
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
            temp_dir: std::env::temp_dir(),
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    #[cfg(test)]
    /// Allow to use a custom temporary directory to avoid conflicts during the snapshot verification.
    pub fn set_sub_temp_dir<T: Into<String>>(&mut self, sub_dir: T) {
        self.temp_dir = mithril_common::test_utils::TempDir::create("snapshotter-temp", sub_dir);
    }

    fn snapshot<T: TarAppender>(&self, filepath: &Path, appender: T) -> StdResult<OngoingSnapshot> {
        let temporary_archive_path = self
            .ongoing_snapshot_directory
            .join(filepath.with_extension("tmp"));
        let archive_path = self.ongoing_snapshot_directory.join(filepath);
        if let Some(archive_dir) = archive_path.parent() {
            fs::create_dir_all(archive_dir).with_context(|| {
                format!(
                    "Can not create archive directory: '{}'",
                    archive_dir.display()
                )
            })?;
        }
        let filesize = self
            .create_and_verify_archive(&temporary_archive_path, appender)
            .inspect_err(|_err| {
                if temporary_archive_path.exists() {
                    if let Err(remove_error) = fs::remove_file(&temporary_archive_path) {
                        warn!(
                            self.logger, " > Post snapshotter.snapshot failure, could not remove temporary archive";
                            "archive_path" => temporary_archive_path.display(),
                            "error" => remove_error
                        );
                    }
                }
            })
            .with_context(|| {
                format!(
                    "CompressedArchiveSnapshotter can not create and verify archive: '{}'",
                    archive_path.display()
                )
            })?;

        fs::rename(&temporary_archive_path, &archive_path).with_context(|| {
            format!(
                "CompressedArchiveSnapshotter can not rename temporary archive: '{}' to final archive: '{}'",
                temporary_archive_path.display(),
                archive_path.display()
            )
        })?;

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

        let unpack_temp_dir = self
            .temp_dir
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use uuid::Uuid;

    use mithril_common::digesters::DummyCardanoDbBuilder;

    use mithril_common::test_utils::assert_equivalent;

    use crate::services::snapshotter::test_tools::*;
    use crate::test_tools::TestLogger;
    use crate::ZstandardCompressionParameters;

    use super::*;

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
    fn should_not_delete_an_alreay_existing_archive_with_same_name_if_snapshotting_fail() {
        let test_dir = get_test_directory(
            "should_not_delete_an_alreay_existing_archive_with_same_name_if_snapshotting_fail",
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
        create_file(&pending_snapshot_directory, "other-process.file");
        create_file(&pending_snapshot_directory, "whatever.tar.gz");
        // an already existing temporary archive file should be deleted
        create_file(&pending_snapshot_directory, "whatever.tar.tmp");

        let _ = snapshotter
            .snapshot_all(Path::new("whatever.tar.gz"))
            .expect_err("Snapshotter::snapshot should fail if the db is empty.");
        let remaining_files: Vec<String> = fs::read_dir(&pending_snapshot_directory)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect();

        assert_equivalent(
            vec![
                "other-process.file".to_string(),
                "whatever.tar.gz".to_string(),
            ],
            remaining_files,
        );
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

        let mut snapshotter = CompressedArchiveSnapshotter::new(
            db_directory.clone(),
            pending_snapshot_directory.clone(),
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();
        snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

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

        let mut snapshotter = CompressedArchiveSnapshotter::new(
            db_directory.clone(),
            pending_snapshot_directory.clone(),
            ZstandardCompressionParameters::default().into(),
            TestLogger::stdout(),
        )
        .unwrap();
        snapshotter.set_sub_temp_dir(Uuid::new_v4().to_string());

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
    fn snapshot_overwrite_archive_already_existing() {
        let test_dir = get_test_directory("snapshot_overwrite_archive_already_existing");
        let destination = test_dir.join(create_dir(&test_dir, "destination"));
        let source = test_dir.join(create_dir(&test_dir, "source"));

        create_file(&source, "file_to_archive.txt");

        let snapshotter = CompressedArchiveSnapshotter::new(
            source.clone(),
            destination,
            SnapshotterCompressionAlgorithm::Gzip,
            TestLogger::stdout(),
        )
        .unwrap();

        let first_snapshot = snapshotter
            .snapshot_all(Path::new(&random_archive_name()))
            .unwrap();
        let first_snapshot_size = first_snapshot.get_file_size();

        create_file(&source, "another_file_to_archive.txt");

        let second_snapshot = snapshotter
            .snapshot_all(Path::new(&random_archive_name()))
            .unwrap();
        let second_snapshot_size = second_snapshot.get_file_size();

        assert_ne!(first_snapshot_size, second_snapshot_size);

        let unpack_path = unpack_gz_decoder(test_dir, second_snapshot);
        assert!(unpack_path.join("another_file_to_archive.txt").exists());
    }

    #[test]
    fn can_set_temp_dir_with_str_or_string() {
        let mut snapshotter = CompressedArchiveSnapshotter {
            db_directory: Default::default(),
            ongoing_snapshot_directory: Default::default(),
            compression_algorithm: SnapshotterCompressionAlgorithm::Gzip,
            temp_dir: Default::default(),
            logger: TestLogger::stdout(),
        };

        snapshotter.set_sub_temp_dir("sub_dir");
        snapshotter.set_sub_temp_dir("sub_dir".to_string());
    }
}
