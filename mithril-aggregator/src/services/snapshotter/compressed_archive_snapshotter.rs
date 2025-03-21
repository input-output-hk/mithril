use anyhow::{anyhow, Context};
use slog::{debug, Logger};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::digesters::{
    immutable_trio_names, ImmutableFile, LedgerFile, IMMUTABLE_DIR, LEDGER_DIR,
};
use mithril_common::entities::{CompressionAlgorithm, ImmutableFileNumber};
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::dependency_injection::DependenciesBuilderError;
use crate::tools::file_archiver::appender::{AppenderEntries, TarAppender};
use crate::tools::file_archiver::{ArchiveParameters, FileArchive, FileArchiver};

use super::Snapshotter;

/// Compressed Archive Snapshotter create a compressed file.
pub struct CompressedArchiveSnapshotter {
    /// DB directory to snapshot
    db_directory: PathBuf,

    /// Directory to store ongoing snapshot
    ongoing_snapshot_directory: PathBuf,

    /// Compression algorithm to use
    compression_algorithm: CompressionAlgorithm,

    file_archiver: Arc<FileArchiver>,

    logger: Logger,
}

impl Snapshotter for CompressedArchiveSnapshotter {
    fn snapshot_all_completed_immutables(
        &self,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        debug!(
            self.logger,
            "Snapshotting all completed immutables into archive: '{archive_name_without_extension}'"
        );

        let paths_to_include = ImmutableFile::list_completed_in_dir(&self.db_directory)
            .with_context(|| {
                format!(
                    "Can not list completed immutables in database directory: '{}'",
                    self.db_directory.display()
                )
            })?
            .into_iter()
            .map(|immutable_file: ImmutableFile| {
                PathBuf::from(IMMUTABLE_DIR).join(immutable_file.filename)
            })
            .collect();
        let appender = AppenderEntries::new(paths_to_include, self.db_directory.clone())?;
        self.snapshot(archive_name_without_extension, appender)
    }

    fn snapshot_ancillary(
        &self,
        immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        debug!(
            self.logger,
            "Snapshotting ancillary archive: '{archive_name_without_extension}'"
        );

        let paths_to_include =
            self.get_files_and_directories_for_ancillary_snapshot(immutable_file_number)?;
        let appender = AppenderEntries::new(paths_to_include, self.db_directory.clone())?;
        self.snapshot(archive_name_without_extension, appender)
    }

    fn snapshot_immutable_trio(
        &self,
        immutable_file_number: ImmutableFileNumber,
        archive_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        debug!(
            self.logger,
            "Snapshotting immutable trio {immutable_file_number} into archive '{archive_name_without_extension}'"
        );

        let files_to_archive = immutable_trio_names(immutable_file_number)
            .iter()
            .map(|filename| PathBuf::from(IMMUTABLE_DIR).join(filename))
            .collect();
        let appender = AppenderEntries::new(files_to_archive, self.db_directory.clone())?;

        self.snapshot(archive_name_without_extension, appender)
    }

    fn compression_algorithm(&self) -> CompressionAlgorithm {
        self.compression_algorithm
    }
}

impl CompressedArchiveSnapshotter {
    /// Snapshotter factory
    pub fn new(
        db_directory: PathBuf,
        ongoing_snapshot_directory: PathBuf,
        compression_algorithm: CompressionAlgorithm,
        file_archiver: Arc<FileArchiver>,
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
            file_archiver,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    fn snapshot<T: TarAppender>(
        &self,
        name_without_extension: &str,
        appender: T,
    ) -> StdResult<FileArchive> {
        self.file_archiver.archive(
            ArchiveParameters {
                archive_name_without_extension: name_without_extension.to_string(),
                target_directory: self.ongoing_snapshot_directory.clone(),
                compression_algorithm: self.compression_algorithm,
            },
            appender,
        )
    }

    /// Returns the list of files and directories to include in ancillary snapshot.
    ///
    /// The immutable file included in the ancillary archive corresponds to the last one (and not finalized yet)
    /// when the immutable file number given to the function corresponds to the penultimate.
    fn get_files_and_directories_for_ancillary_snapshot(
        &self,
        immutable_file_number: u64,
    ) -> StdResult<Vec<PathBuf>> {
        let next_immutable_file_number = immutable_file_number + 1;
        let mut files_to_snapshot: Vec<PathBuf> = immutable_trio_names(next_immutable_file_number)
            .into_iter()
            .map(|filename| PathBuf::from(IMMUTABLE_DIR).join(filename))
            .collect();

        let db_ledger_dir = self.db_directory.join(LEDGER_DIR);
        let ledger_files = LedgerFile::list_all_in_dir(&db_ledger_dir)?;
        let last_ledger = ledger_files.last().ok_or(anyhow!(
            "No ledger file found in directory: `{}`",
            db_ledger_dir.display()
        ))?;
        files_to_snapshot.push(PathBuf::from(LEDGER_DIR).join(&last_ledger.filename));

        Ok(files_to_snapshot)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::path::Path;
    use std::sync::Arc;

    use mithril_common::digesters::DummyCardanoDbBuilder;
    use mithril_common::temp_dir_create;
    use mithril_common::test_utils::assert_equivalent;

    use crate::services::snapshotter::test_tools::*;
    use crate::test_tools::TestLogger;

    use super::*;

    fn list_files(test_dir: &Path) -> Vec<String> {
        fs::read_dir(test_dir)
            .unwrap()
            .map(|f| f.unwrap().file_name().to_str().unwrap().to_owned())
            .collect()
    }

    fn snapshotter_for_test(
        test_directory: &Path,
        db_directory: &Path,
        compression_algorithm: CompressionAlgorithm,
    ) -> CompressedArchiveSnapshotter {
        CompressedArchiveSnapshotter::new(
            db_directory.to_path_buf(),
            test_directory.join("ongoing_snapshot"),
            compression_algorithm,
            Arc::new(FileArchiver::new_for_test(
                test_directory.join("verification"),
            )),
            TestLogger::stdout(),
        )
        .unwrap()
    }

    #[test]
    fn return_parametrized_compression_algorithm() {
        let test_dir = get_test_directory("return_parametrized_compression_algorithm");
        let snapshotter = snapshotter_for_test(
            &test_dir,
            Path::new("whatever"),
            CompressionAlgorithm::Zstandard,
        );

        assert_eq!(
            CompressionAlgorithm::Zstandard,
            snapshotter.compression_algorithm()
        );
    }

    #[test]
    fn should_create_directory_if_does_not_exist() {
        let test_dir = get_test_directory("should_create_directory_if_does_not_exist");
        let ongoing_snapshot_directory = test_dir.join("ongoing_snapshot");
        let db_directory = test_dir.join("whatever");

        CompressedArchiveSnapshotter::new(
            db_directory,
            ongoing_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        assert!(ongoing_snapshot_directory.is_dir());
    }

    #[test]
    fn should_clean_ongoing_snapshot_directory_if_already_exists() {
        let test_dir =
            get_test_directory("should_clean_ongoing_snapshot_directory_if_already_exists");
        let ongoing_snapshot_directory = test_dir.join("ongoing_snapshot");
        let db_directory = test_dir.join("whatever");

        fs::create_dir_all(&ongoing_snapshot_directory).unwrap();

        File::create(ongoing_snapshot_directory.join("whatever.txt")).unwrap();

        CompressedArchiveSnapshotter::new(
            db_directory,
            ongoing_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();

        assert_eq!(0, fs::read_dir(ongoing_snapshot_directory).unwrap().count());
    }

    #[test]
    fn should_create_snapshots_in_its_ongoing_snapshot_directory() {
        let test_dir = temp_dir_create!();
        let pending_snapshot_directory = test_dir.join("pending_snapshot");
        let cardano_db =
            DummyCardanoDbBuilder::new("should_create_snapshots_in_its_ongoing_snapshot_directory")
                .with_immutables(&[1])
                .append_immutable_trio()
                .build();

        let snapshotter = CompressedArchiveSnapshotter::new(
            cardano_db.get_dir().clone(),
            pending_snapshot_directory.clone(),
            CompressionAlgorithm::Gzip,
            Arc::new(FileArchiver::new_for_test(test_dir.join("verification"))),
            TestLogger::stdout(),
        )
        .unwrap();
        let snapshot = snapshotter
            .snapshot_all_completed_immutables("whatever")
            .unwrap();

        assert_eq!(
            pending_snapshot_directory,
            snapshot.get_file_path().parent().unwrap()
        );
    }

    mod snapshot_all_completed_immutables {
        use super::*;

        #[test]
        fn include_only_completed_immutables() {
            let test_dir = temp_dir_create!();
            let cardano_db = DummyCardanoDbBuilder::new(
                "snapshot_all_immutables_include_only_completed_immutables",
            )
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .with_ledger_files(&["437"])
            .with_volatile_files(&["blocks-0.dat"])
            .with_non_immutables(&["random_file.txt", "00002.trap"])
            .build();

            let snapshotter =
                snapshotter_for_test(&test_dir, cardano_db.get_dir(), CompressionAlgorithm::Gzip);

            let snapshot = snapshotter
                .snapshot_all_completed_immutables("completed_immutables")
                .unwrap();

            let unpack_dir = snapshot.unpack_gzip(&test_dir);
            let unpacked_files = list_files(&unpack_dir);
            let unpacked_immutable_files = list_files(&unpack_dir.join(IMMUTABLE_DIR));

            assert_equivalent(vec![IMMUTABLE_DIR.to_string()], unpacked_files);
            assert_equivalent(
                vec![
                    "00001.chunk".to_string(),
                    "00001.primary".to_string(),
                    "00001.secondary".to_string(),
                    "00002.chunk".to_string(),
                    "00002.primary".to_string(),
                    "00002.secondary".to_string(),
                    "00003.chunk".to_string(),
                    "00003.primary".to_string(),
                    "00003.secondary".to_string(),
                ],
                unpacked_immutable_files,
            );
        }
    }

    mod snapshot_immutable_trio {
        use super::*;

        #[test]
        fn include_only_immutable_trio() {
            let test_dir = get_test_directory("include_only_immutable_trio");
            let cardano_db = DummyCardanoDbBuilder::new("include_only_immutable_trio")
                .with_immutables(&[1, 2, 3])
                .with_ledger_files(&["437"])
                .with_volatile_files(&["blocks-0.dat"])
                .with_non_immutables(&["random_file.txt", "00002.trap"])
                .build();

            let snapshotter =
                snapshotter_for_test(&test_dir, cardano_db.get_dir(), CompressionAlgorithm::Gzip);

            let snapshot = snapshotter
                .snapshot_immutable_trio(2, "immutable-2")
                .unwrap();

            let unpack_dir = snapshot.unpack_gzip(&test_dir);
            let unpacked_files = list_files(&unpack_dir);
            let unpacked_immutable_files = list_files(&unpack_dir.join(IMMUTABLE_DIR));

            assert_equivalent(vec![IMMUTABLE_DIR.to_string()], unpacked_files);
            assert_equivalent(
                vec![
                    "00002.chunk".to_string(),
                    "00002.primary".to_string(),
                    "00002.secondary".to_string(),
                ],
                unpacked_immutable_files,
            );
        }
    }

    mod snapshot_ancillary {
        use mithril_common::digesters::VOLATILE_DIR;

        use super::*;

        #[tokio::test]
        async fn create_archive_should_embed_only_last_ledger_and_last_immutables() {
            let test_dir = get_test_directory(
                "create_archive_should_embed_only_last_ledger_and_last_immutables",
            );
            let cardano_db = DummyCardanoDbBuilder::new(
                "create_archive_should_embed_only_last_ledger_and_last_immutables",
            )
            .with_immutables(&[1, 2, 3])
            .with_ledger_files(&["437", "537", "637", "737", "9not_included"])
            .with_volatile_files(&["blocks-0.dat", "blocks-1.dat", "blocks-2.dat"])
            .build();
            fs::create_dir(cardano_db.get_dir().join("whatever")).unwrap();

            let db_directory = cardano_db.get_dir();
            let snapshotter =
                snapshotter_for_test(&test_dir, db_directory, CompressionAlgorithm::Gzip);

            let snapshot = snapshotter.snapshot_ancillary(2, "ancillary").unwrap();

            let unpack_dir = snapshot.unpack_gzip(&test_dir);

            let expected_immutable_path = unpack_dir.join(IMMUTABLE_DIR);
            assert!(expected_immutable_path.join("00003.chunk").exists());
            assert!(expected_immutable_path.join("00003.primary").exists());
            assert!(expected_immutable_path.join("00003.secondary").exists());
            assert_eq!(3, list_files(&expected_immutable_path).len());

            // Only the last ledger file should be included
            let expected_ledger_path = unpack_dir.join(LEDGER_DIR);
            assert!(expected_ledger_path.join("737").exists());
            assert_eq!(1, list_files(&expected_ledger_path).len());

            let expected_volatile_path = unpack_dir.join(VOLATILE_DIR);
            assert!(!expected_volatile_path.exists());

            assert!(!unpack_dir.join("whatever").exists());
        }
    }
}
