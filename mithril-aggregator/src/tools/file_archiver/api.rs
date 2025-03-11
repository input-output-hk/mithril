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

use super::appender::TarAppender;
use super::{FileArchive, FileArchiverCompressionAlgorithm};

/// Tool to archive files and directories.
pub struct FileArchiver {
    compression_algorithm: FileArchiverCompressionAlgorithm,
    // Temporary directory to  the unpacked archive for verification
    verification_temp_dir: PathBuf,
    logger: Logger,
}

impl FileArchiver {
    /// Constructs a new `FileArchiver`.
    pub fn new(
        compression_algorithm: FileArchiverCompressionAlgorithm,
        verification_temp_dir: PathBuf,
        logger: Logger,
    ) -> Self {
        Self {
            compression_algorithm,
            verification_temp_dir,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Archive the content of a directory.
    pub fn archive<T: TarAppender>(
        &self,
        target_path: &Path,
        appender: T,
    ) -> StdResult<FileArchive> {
        let temporary_archive_path = target_path.with_extension("tmp");
        if let Some(archive_dir) = target_path.parent() {
            fs::create_dir_all(archive_dir).with_context(|| {
                format!(
                    "FileArchiver can not create archive directory: '{}'",
                    archive_dir.display()
                )
            })?;
        }

        let temporary_file_archive = self
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
                    "FileArchiver can not create and verify archive: '{}'",
                    target_path.display()
                )
            })?;

        fs::rename(&temporary_archive_path, target_path).with_context(|| {
            format!(
                "FileArchiver can not rename temporary archive: '{}' to final archive: '{}'",
                temporary_archive_path.display(),
                target_path.display()
            )
        })?;

        Ok(FileArchive {
            filepath: target_path.to_path_buf(),
            filesize: temporary_file_archive.filesize,
        })
    }

    fn create_and_verify_archive<T: TarAppender>(
        &self,
        archive_path: &Path,
        appender: T,
    ) -> StdResult<FileArchive> {
        let file_archive = self
            .create_archive(archive_path, appender)
            .with_context(|| {
                format!(
                    "FileArchiver can not create archive with path: '{}''",
                    archive_path.display()
                )
            })?;
        self.verify_archive(&file_archive).with_context(|| {
            format!(
                "FileArchiver can not verify archive with path: '{}''",
                archive_path.display()
            )
        })?;

        Ok(file_archive)
    }

    fn create_archive<T: TarAppender>(
        &self,
        archive_path: &Path,
        appender: T,
    ) -> StdResult<FileArchive> {
        info!(
            self.logger,
            "Archiving content to archive: '{}'",
            archive_path.display()
        );

        let tar_file = File::create(archive_path).with_context(|| {
            format!("Error while creating the archive with path: {archive_path:?}")
        })?;

        match self.compression_algorithm {
            FileArchiverCompressionAlgorithm::Gzip => {
                let enc = GzEncoder::new(tar_file, Compression::default());
                let mut tar = tar::Builder::new(enc);

                appender
                    .append(&mut tar)
                    .with_context(|| "GzEncoder Builder failed to append content")?;

                let mut gz = tar
                    .into_inner()
                    .with_context(|| "GzEncoder Builder can not write the archive")?;
                gz.try_finish()
                    .with_context(|| "GzEncoder can not finish the output stream after writing")?;
            }
            FileArchiverCompressionAlgorithm::Zstandard(params) => {
                let mut enc = Encoder::new(tar_file, params.level)?;
                enc.multithread(params.number_of_workers)
                    .with_context(|| "ZstandardEncoder can not set the number of workers")?;
                let mut tar = tar::Builder::new(enc);

                appender
                    .append(&mut tar)
                    .with_context(|| "ZstandardEncoder Builder failed to append content")?;

                let zstd = tar
                    .into_inner()
                    .with_context(|| "ZstandardEncoder Builder can not write the archive")?;
                zstd.finish().with_context(|| {
                    "ZstandardEncoder can not finish the output stream after writing"
                })?;
            }
        }

        let filesize = Self::get_file_size(archive_path).with_context(|| {
            format!(
                "FileArchiver can not get file size of archive with path: '{}'",
                archive_path.display()
            )
        })?;

        Ok(FileArchive {
            filepath: archive_path.to_path_buf(),
            filesize,
        })
    }

    /// Verify the integrity of the archive.
    fn verify_archive(&self, archive: &FileArchive) -> StdResult<()> {
        info!(
            self.logger,
            "Verifying archive: {}",
            archive.filepath.display()
        );

        let mut snapshot_file_tar = File::open(&archive.filepath).with_context(|| {
            format!(
                "Verify archive error: can not open archive: '{}'",
                archive.filepath.display()
            )
        })?;
        snapshot_file_tar.seek(SeekFrom::Start(0))?;

        let mut snapshot_archive: Archive<Box<dyn Read>> = match self.compression_algorithm {
            FileArchiverCompressionAlgorithm::Gzip => {
                let snapshot_file_tar = GzDecoder::new(snapshot_file_tar);
                Archive::new(Box::new(snapshot_file_tar))
            }
            FileArchiverCompressionAlgorithm::Zstandard(_) => {
                let snapshot_file_tar = Decoder::new(snapshot_file_tar)?;
                Archive::new(Box::new(snapshot_file_tar))
            }
        };

        let unpack_temp_dir = self
            .verification_temp_dir
            .join("mithril_archiver_verify_archive")
            // Add the archive name to the directory to allow two verifications at the same time
            // (useful for tests).
            .join(archive.filepath.file_name().ok_or(anyhow!(
                "Verify archive error: Could not append archive name to temp directory: archive `{}`",
                archive.filepath.display(),
            ))?);

        fs::create_dir_all(&unpack_temp_dir).with_context(|| {
            format!(
                "Verify archive error: Could not create directory `{}`",
                unpack_temp_dir.display(),
            )
        })?;

        let unpack_temp_file = &unpack_temp_dir.join("unpack.tmp");

        let verify_result = {
            let mut result = Ok(());
            for e in snapshot_archive.entries()? {
                match e {
                    Err(e) => {
                        result = Err(anyhow!(e).context("Verify archive error: invalid entry"));
                        break;
                    }
                    Ok(entry) => Self::unpack_and_delete_file_from_entry(entry, unpack_temp_file)?,
                };
            }
            result
        };

        // Always remove the temp directory
        fs::remove_dir_all(&unpack_temp_dir).with_context(|| {
            format!(
                "Verify archive error: Could not remove directory `{}`",
                unpack_temp_dir.display()
            )
        })?;

        verify_result
    }

    fn get_file_size(filepath: &Path) -> StdResult<u64> {
        let res = fs::metadata(filepath)
            .with_context(|| {
                format!(
                    "FileArchiver can not get metadata of file: '{}'",
                    filepath.display()
                )
            })?
            .len();
        Ok(res)
    }

    // Helper to unpack and delete a file from en entry, for archive verification purpose
    fn unpack_and_delete_file_from_entry<R: Read>(
        entry: Entry<R>,
        unpack_file_path: &Path,
    ) -> StdResult<()> {
        if entry.header().entry_type() != EntryType::Directory {
            let mut file = entry;
            let _ = file
                .unpack(unpack_file_path)
                .with_context(|| "can't unpack entry")?;

            fs::remove_file(unpack_file_path).with_context(|| {
                format!(
                    "can't remove temporary unpacked file, file path: `{}`",
                    unpack_file_path.display()
                )
            })?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use mithril_common::test_utils::assert_equivalent;

    use crate::test_tools::TestLogger;
    use crate::tools::file_archiver::appender::AppenderDirAll;
    use crate::tools::file_archiver::test_tools::*;
    use crate::ZstandardCompressionParameters;

    use super::*;

    fn list_remaining_files(test_dir: &Path) -> Vec<String> {
        fs::read_dir(&test_dir)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect()
    }

    #[test]
    fn should_create_a_valid_archive_with_gzip_compression() {
        let test_dir = get_test_directory("should_create_a_valid_archive_with_gzip_compression");
        let target_archive = test_dir.join("archive.tar.gz");
        let archived_directory = test_dir.join(create_dir(&test_dir, "archived_directory"));
        create_file(&archived_directory, "file_to_archive.txt");

        let file_archiver = FileArchiver::new(
            FileArchiverCompressionAlgorithm::Gzip,
            test_dir.join("verification_temp_dir"),
            TestLogger::stdout(),
        );

        let archive = file_archiver
            .create_archive(&target_archive, AppenderDirAll::new(archived_directory))
            .expect("create_archive should not fail");
        file_archiver
            .verify_archive(&archive)
            .expect("verify_archive should not fail");
    }

    #[test]
    fn should_create_a_valid_archive_with_zstandard_compression() {
        let test_dir =
            get_test_directory("should_create_a_valid_archive_with_zstandard_compression");
        let target_archive = test_dir.join("archive.tar.gz");
        let archived_directory = test_dir.join(create_dir(&test_dir, "archived_directory"));
        create_file(&archived_directory, "file_to_archive.txt");

        let file_archiver = FileArchiver::new(
            ZstandardCompressionParameters::default().into(),
            test_dir.join("verification_temp_dir"),
            TestLogger::stdout(),
        );

        let archive = file_archiver
            .create_archive(&target_archive, AppenderDirAll::new(archived_directory))
            .expect("create_archive should not fail");
        file_archiver
            .verify_archive(&archive)
            .expect("verify_archive should not fail");
    }

    #[test]
    fn should_delete_tmp_file_in_target_directory_if_archiving_fail() {
        let test_dir =
            get_test_directory("should_delete_tmp_file_in_target_directory_if_archiving_fail");
        // Note: the archived directory does not exist in order to make the archive process fail
        let archived_directory = test_dir.join("db");
        let target_archive = test_dir.join("archive.tar.gz");

        let file_archiver = FileArchiver::new(
            FileArchiverCompressionAlgorithm::Gzip,
            test_dir.join("verification_temp_dir"),
            TestLogger::stdout(),
        );

        // this file should not be deleted by the archive creation
        File::create(test_dir.join("other-process.file")).unwrap();

        let _ = file_archiver
            .archive(&target_archive, AppenderDirAll::new(archived_directory))
            .expect_err("FileArchiver::archive should fail if the target path doesn't exist.");

        let remaining_files: Vec<String> = list_remaining_files(&test_dir);
        assert_eq!(vec!["other-process.file".to_string()], remaining_files);
    }

    #[test]
    fn should_not_delete_an_already_existing_archive_with_same_name_if_archiving_fail() {
        let test_dir = get_test_directory(
            "should_not_delete_an_already_existing_archive_with_same_name_if_archiving_fail",
        );
        // Note: the archived directory does not exist in order to make the archive process fail
        let archived_directory = test_dir.join("db");
        let target_archive = test_dir.join("archive.tar.gz");

        let file_archiver = FileArchiver::new(
            FileArchiverCompressionAlgorithm::Gzip,
            test_dir.join("verification_temp_dir"),
            TestLogger::stdout(),
        );

        // this file should not be deleted by the archive creation
        create_file(&test_dir, "other-process.file");
        create_file(&test_dir, "archive.tar.gz");
        // an already existing temporary archive file should be deleted
        create_file(&test_dir, "archive.tar.tmp");

        let _ = file_archiver
            .archive(&target_archive, AppenderDirAll::new(archived_directory))
            .expect_err("FileArchiver::archive should fail if the db is empty.");
        let remaining_files: Vec<String> = list_remaining_files(&test_dir);

        assert_equivalent(
            vec![
                "other-process.file".to_string(),
                "archive.tar.gz".to_string(),
            ],
            remaining_files,
        );
    }

    #[test]
    fn overwrite_already_existing_archive_when_archiving_succeed() {
        let test_dir =
            get_test_directory("overwrite_already_existing_archive_when_archiving_succeed");
        let archived_directory = test_dir.join(create_dir(&test_dir, "archived_directory"));
        let target_archive = test_dir.join("archive.tar.gz");

        create_file(&archived_directory, "file_to_archive.txt");

        let file_archiver = FileArchiver::new(
            FileArchiverCompressionAlgorithm::Gzip,
            test_dir.join("verification_temp_dir"),
            TestLogger::stdout(),
        );

        let first_snapshot = file_archiver
            .archive(
                &target_archive,
                AppenderDirAll::new(archived_directory.clone()),
            )
            .unwrap();
        let first_snapshot_size = first_snapshot.get_file_size();

        create_file(&archived_directory, "another_file_to_archive.txt");

        let second_snapshot = file_archiver
            .archive(&target_archive, AppenderDirAll::new(archived_directory))
            .unwrap();
        let second_snapshot_size = second_snapshot.get_file_size();

        assert_ne!(first_snapshot_size, second_snapshot_size);

        let unpack_path = unpack_gz_decoder(test_dir, second_snapshot);
        assert!(unpack_path.join("another_file_to_archive.txt").exists());
    }
}
