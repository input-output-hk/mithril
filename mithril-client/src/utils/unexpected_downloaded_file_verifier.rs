//! Toolbox to verify if unexpected files are included when downloading and unpacking
//! Mithril archives and delete found offenders.
//!
//! This reduces the ability of adversarial aggregators to leverage Mithril archives for side
//! channel attacks.
//!
//! Requirements:
//! * Existing extra files added by users should be kept
//! * Found offenders should be reported
//!
use std::collections::HashSet;
use std::ffi::OsString;
use std::ops::RangeInclusive;
use std::path::{Path, PathBuf};

use anyhow::Context;
use slog::Logger;

use mithril_common::digesters::{immutable_trio_names, IMMUTABLE_DIR};
use mithril_common::entities::ImmutableFileNumber;
use mithril_common::StdResult;

const BASE_ERROR: &str = "Unexpected downloaded file check failed";

/// Tool to check and remove unexpected files when downloading and unpacking Mithril archives
pub struct UnexpectedDownloadedFileVerifier {
    target_cardano_db_dir: PathBuf,
    immutable_files_range_to_expect: RangeInclusive<ImmutableFileNumber>,
    logger: Logger,
}

/// List of expected files after downloading and unpacking, yielded by `UnexpectedDownloadedFileVerifier::compute_expected_state_after_download`
pub struct ExpectedFilesAfterDownload {
    target_cardano_db_dir: PathBuf,
    expected_filenames_in_immutable_dir: HashSet<OsString>,
    logger: Logger,
}

impl UnexpectedDownloadedFileVerifier {
    /// `UnexpectedDownloadedFileVerifier` factory
    pub fn new<P: AsRef<Path>>(
        target_cardano_db_dir: P,
        network_kind: &str,
        include_ancillary: bool,
        last_downloaded_immutable_file_number: ImmutableFileNumber,
        logger: &Logger,
    ) -> Self {
        Self {
            target_cardano_db_dir: target_cardano_db_dir.as_ref().to_path_buf(),
            immutable_files_range_to_expect: compute_immutable_files_range_to_expect(
                network_kind,
                include_ancillary,
                last_downloaded_immutable_file_number,
            ),
            logger: logger.clone(),
        }
    }

    /// Compute the expected state of the folder after download finish
    pub async fn compute_expected_state_after_download(
        &self,
    ) -> StdResult<ExpectedFilesAfterDownload> {
        let immutable_files_dir = self.target_cardano_db_dir.join(IMMUTABLE_DIR);
        let immutable_files_range_to_expect = self.immutable_files_range_to_expect.clone();
        // target databases can be quite large, avoid blocking the main thread
        let expected_files =
            tokio::task::spawn_blocking(move || -> StdResult<HashSet<OsString>> {
                let mut files: HashSet<OsString> = if immutable_files_dir.exists() {
                    std::fs::read_dir(&immutable_files_dir)
                        .with_context(|| {
                            format!("Failed to read directory {}", immutable_files_dir.display())
                        })?
                        .flat_map(|e| e.map(|e| e.file_name()))
                        .collect()
                } else {
                    HashSet::new()
                };

                // Complete the list with all rightfully downloaded immutable files
                for immutable_file_name in
                    immutable_files_range_to_expect.flat_map(immutable_trio_names)
                {
                    files.insert(OsString::from(immutable_file_name));
                }
                Ok(files)
            })
            .await?
            .with_context(|| BASE_ERROR)?;

        Ok(ExpectedFilesAfterDownload {
            target_cardano_db_dir: self.target_cardano_db_dir.clone(),
            expected_filenames_in_immutable_dir: expected_files,
            logger: self.logger.clone(),
        })
    }
}

fn compute_immutable_files_range_to_expect(
    network_kind: &str,
    include_ancillary: bool,
    last_downloaded_immutable_file_number: ImmutableFileNumber,
) -> RangeInclusive<ImmutableFileNumber> {
    let is_devnet_network = network_kind.contains("devnet");
    let lower_bound = if is_devnet_network { 0 } else { 1 };
    let upper_bound = if include_ancillary {
        last_downloaded_immutable_file_number + 1
    } else {
        last_downloaded_immutable_file_number
    };

    lower_bound..=upper_bound
}

impl ExpectedFilesAfterDownload {
    /// Identify and delete unexpected files and folder
    ///
    /// Returns the name of the deleted items relative to the target download directory and
    /// write a warning to the logger.
    ///
    /// *Note: removed directories names are suffixed with a "/"*
    pub async fn remove_unexpected_files(self) -> StdResult<Option<Vec<String>>> {
        tokio::task::spawn_blocking(move || {
            let unexpected_entries_in_immutable_dir: Vec<_> =
                std::fs::read_dir(self.target_cardano_db_dir.join(IMMUTABLE_DIR))
                    .with_context(|| BASE_ERROR)?
                    .flatten()
                    .filter(|entry| {
                        !self
                            .expected_filenames_in_immutable_dir
                            .contains(&entry.file_name())
                    })
                    .collect();
            let mut removed_entries = Vec::new();

            for unexpected_entry in &unexpected_entries_in_immutable_dir {
                let unexpected_path = unexpected_entry.path();
                if unexpected_path.is_dir() {
                    std::fs::remove_dir_all(&unexpected_path)
                        .with_context(|| {
                            format!(
                                "failed to remove unexpected directory `{}`",
                                unexpected_path.display()
                            )
                        })
                        .with_context(|| BASE_ERROR)?;
                    // Join a "/" to the end to make explicit that it's a directory
                    removed_entries.push(format!(
                        "{IMMUTABLE_DIR}/{}/",
                        unexpected_entry.file_name().to_string_lossy()
                    ));
                } else {
                    std::fs::remove_file(&unexpected_path)
                        .with_context(|| {
                            format!(
                                "failed to remove unexpected file `{}`",
                                unexpected_path.display()
                            )
                        })
                        .with_context(|| BASE_ERROR)?;
                    removed_entries.push(format!(
                        "{IMMUTABLE_DIR}/{}",
                        unexpected_entry.file_name().to_string_lossy()
                    ));
                }
            }

            if removed_entries.is_empty() {
                Ok(None)
            } else {
                // Sort removed entries to ensure consistent output when reporting to users
                removed_entries.sort();

                slog::warn!(
                    self.logger, "Unexpected files removed after downloads";
                    "directory" => self.target_cardano_db_dir.display(),
                    "removed_entries" => ?removed_entries
                );
                Ok(Some(removed_entries))
            }
        })
        .await?
    }
}

#[cfg(test)]
mod tests {
    use std::fs::{create_dir, File};
    use std::time::Instant;

    use mithril_common::temp_dir_create;

    use crate::test_utils::TestLogger;

    use super::*;

    fn create_immutable_files_dir(parent_dir: &Path) -> PathBuf {
        let immutable_files_dir = parent_dir.join(IMMUTABLE_DIR);
        create_dir(&immutable_files_dir).unwrap();
        immutable_files_dir
    }

    fn create_immutable_trio(dir: &Path, immutable_file_number: ImmutableFileNumber) {
        for immutable_file_name in immutable_trio_names(immutable_file_number) {
            File::create(dir.join(immutable_file_name)).unwrap();
        }
    }

    #[test]
    fn test_compute_immutable_files_range_to_expect() {
        // Specs:
        // - start at 1 on all networks except 0 for devnet
        // - if ancillaries are included, the end bound must be increased by one (to take in an
        // account the additional immutable trio downloaded with them)

        // Without ancillaries, network is not devnet
        assert_eq!(
            compute_immutable_files_range_to_expect("network", false, 143),
            1..=143
        );

        // Without ancillaries, network is devnet
        assert_eq!(
            compute_immutable_files_range_to_expect("devnet", false, 143),
            0..=143
        );

        // With ancillaries, network is not devnet
        assert_eq!(
            compute_immutable_files_range_to_expect("network", true, 143),
            1..=144
        );

        // With ancillaries, network is devnet
        assert_eq!(
            compute_immutable_files_range_to_expect("devnet", true, 143),
            0..=144
        );
    }

    mod compute_expected_state_after_download {
        use super::*;

        #[tokio::test]
        async fn when_dir_empty_return_empty_if_immutable_files_dir_does_not_exist_and_range_is_empty(
        ) {
            let temp_dir = temp_dir_create!();
            create_immutable_files_dir(&temp_dir);
            let existing_files = UnexpectedDownloadedFileVerifier::new(
                &temp_dir,
                "network",
                false,
                0,
                &TestLogger::stdout(),
            )
            .compute_expected_state_after_download()
            .await
            .unwrap();

            assert_eq!(
                existing_files.expected_filenames_in_immutable_dir,
                HashSet::<OsString>::new()
            );
        }

        #[tokio::test]
        async fn when_dir_empty_return_empty_if_immutable_files_dir_exist_and_range_is_empty() {
            let temp_dir = temp_dir_create!();
            let existing_files = UnexpectedDownloadedFileVerifier::new(
                &temp_dir,
                "network",
                false,
                0,
                &TestLogger::stdout(),
            )
            .compute_expected_state_after_download()
            .await
            .unwrap();

            assert_eq!(
                existing_files.expected_filenames_in_immutable_dir,
                HashSet::<OsString>::new()
            );
        }

        #[tokio::test]
        async fn when_immutable_files_dir_does_not_exist_return_immutables_trios_if_immutable_files_range_is_not_empty(
        ) {
            let temp_dir = temp_dir_create!();
            let existing_files = UnexpectedDownloadedFileVerifier::new(
                &temp_dir,
                "network",
                false,
                1,
                &TestLogger::stdout(),
            )
            .compute_expected_state_after_download()
            .await
            .unwrap();

            assert_eq!(
                existing_files.expected_filenames_in_immutable_dir,
                HashSet::from([
                    OsString::from("00001.chunk"),
                    OsString::from("00001.primary"),
                    OsString::from("00001.secondary"),
                ])
            );
        }

        #[tokio::test]
        async fn when_immutable_files_dir_empty_return_immutables_trios_if_immutable_files_range_is_not_empty(
        ) {
            let temp_dir = temp_dir_create!();
            create_immutable_files_dir(&temp_dir);
            let existing_files = UnexpectedDownloadedFileVerifier::new(
                &temp_dir,
                "network",
                false,
                1,
                &TestLogger::stdout(),
            )
            .compute_expected_state_after_download()
            .await
            .unwrap();

            assert_eq!(
                existing_files.expected_filenames_in_immutable_dir,
                HashSet::from([
                    OsString::from("00001.chunk"),
                    OsString::from("00001.primary"),
                    OsString::from("00001.secondary"),
                ])
            );
        }

        #[tokio::test]
        async fn add_existing_files_and_dirs_from_immutable_files_dir_to_expected_files() {
            let temp_dir = temp_dir_create!();
            let immutable_files_dir = create_immutable_files_dir(&temp_dir);
            create_dir(immutable_files_dir.join("dir_1")).unwrap();
            create_dir(immutable_files_dir.join("dir_2")).unwrap();
            File::create(immutable_files_dir.join("file_1.txt")).unwrap();
            File::create(immutable_files_dir.join("file_2.txt")).unwrap();
            File::create(immutable_files_dir.join("dir_2").join("file_3.txt")).unwrap();

            let existing_files = UnexpectedDownloadedFileVerifier::new(
                &temp_dir,
                "network",
                false,
                0,
                &TestLogger::stdout(),
            )
            .compute_expected_state_after_download()
            .await
            .unwrap();

            assert_eq!(
                existing_files.expected_filenames_in_immutable_dir,
                HashSet::from([
                    OsString::from("dir_1"),
                    OsString::from("dir_2"),
                    OsString::from("file_1.txt"),
                    OsString::from("file_2.txt"),
                ])
            );
        }
    }

    mod removing_unexpected_files {
        use mithril_common::assert_dir_eq;

        use super::*;

        #[tokio::test]
        async fn when_dir_empty_do_nothing_and_return_none() {
            let temp_dir = temp_dir_create!();
            create_immutable_files_dir(&temp_dir);

            let existing_before = ExpectedFilesAfterDownload {
                target_cardano_db_dir: temp_dir.clone(),
                expected_filenames_in_immutable_dir: HashSet::new(),
                logger: TestLogger::stdout(),
            };

            let removed_entries = existing_before.remove_unexpected_files().await.unwrap();
            assert_eq!(removed_entries, None);
            assert_dir_eq!(&temp_dir, format!("* {IMMUTABLE_DIR}/"));
        }

        #[tokio::test]
        async fn when_no_unexpected_file_and_folder_delete_nothing_and_return_none() {
            let temp_dir = temp_dir_create!();
            let immutable_files_dir = create_immutable_files_dir(&temp_dir);
            create_dir(immutable_files_dir.join("dir_1")).unwrap();
            File::create(immutable_files_dir.join("file_1.txt")).unwrap();

            let existing_before = ExpectedFilesAfterDownload {
                target_cardano_db_dir: temp_dir.clone(),
                expected_filenames_in_immutable_dir: HashSet::from([
                    OsString::from("file_1.txt"),
                    OsString::from("dir_1"),
                ]),
                logger: TestLogger::stdout(),
            };

            let removed_entries = existing_before.remove_unexpected_files().await.unwrap();
            assert_eq!(removed_entries, None);
            assert_dir_eq!(
                &temp_dir,
                format!(
                    "* {IMMUTABLE_DIR}/
                     ** dir_1/
                     ** file_1.txt"
                )
            );
        }

        #[tokio::test]
        async fn when_unexpected_dirs_and_files_are_downloaded_remove_them_return_their_filenames_and_log_offenders(
        ) {
            let temp_dir = temp_dir_create!();
            let immutable_files_dir = create_immutable_files_dir(&temp_dir);
            let (logger, log_inspector) = TestLogger::memory();
            let existing_before = ExpectedFilesAfterDownload {
                target_cardano_db_dir: temp_dir.clone(),
                expected_filenames_in_immutable_dir: HashSet::new(),
                logger,
            };

            create_dir(immutable_files_dir.join("dir_1")).unwrap();
            create_dir(immutable_files_dir.join("dir_2")).unwrap();
            File::create(immutable_files_dir.join("file_1.txt")).unwrap();
            File::create(immutable_files_dir.join("file_2.txt")).unwrap();
            File::create(immutable_files_dir.join("dir_2").join("file_3.txt")).unwrap();

            let removed_entries = existing_before.remove_unexpected_files().await.unwrap();
            assert_eq!(
                removed_entries,
                Some(vec![
                    format!("{IMMUTABLE_DIR}/dir_1/"),
                    format!("{IMMUTABLE_DIR}/dir_2/"),
                    format!("{IMMUTABLE_DIR}/file_1.txt"),
                    format!("{IMMUTABLE_DIR}/file_2.txt")
                ])
            );
            assert_dir_eq!(&temp_dir, format!("* {IMMUTABLE_DIR}/"));
            assert!(
                log_inspector
                    .contains_log(&format!(r#"WARN Unexpected files removed after downloads; removed_entries=["{IMMUTABLE_DIR}/dir_1/", "{IMMUTABLE_DIR}/dir_2/", "{IMMUTABLE_DIR}/file_1.txt", "{IMMUTABLE_DIR}/file_2.txt"]"#)),
                "Expected log message not found, logs: {log_inspector}"
            );
        }
    }

    // Note: this test does not have assertion, it's used is to measure time taken over a large
    // database to ensure that's doable without particular optimization.
    #[ignore]
    #[tokio::test]
    async fn checking_unexpected_file_against_a_large_immutable_directory() {
        let temp_dir = temp_dir_create!();
        let verifier = UnexpectedDownloadedFileVerifier::new(
            &temp_dir,
            "network",
            false,
            19999,
            &TestLogger::stdout(),
        );

        let immutable_files_dir = create_immutable_files_dir(&temp_dir);
        for immutable_file_number in 0..=30000 {
            create_immutable_trio(&immutable_files_dir, immutable_file_number);
        }

        let now = Instant::now();
        let existing_files = verifier
            .compute_expected_state_after_download()
            .await
            .unwrap();
        println!(
            "elapsed time on list_existing_file (30k files): {:?}",
            now.elapsed()
        );

        let now = Instant::now();
        let _removed_entries = existing_files.remove_unexpected_files().await.unwrap();
        println!(
            "elapsed time on remove_unexpected_files (10k files to remove): {:?}",
            now.elapsed()
        );
    }
}
