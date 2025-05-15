//! Toolbox to verify if unexpected files are included when downloading and unpacking
//! Mithril archives and delete found offenders.
//!
//! This reduces the ability of adversarial aggregators to leverage Mithril archives for side
//! channel attacks.
//!
//! Requirements:
//! * Existing extra files added by users should be kept
//! * Found offenders should be reported
//! * Remove safely unexpected symbolic links
//!
use std::path::{Path, PathBuf};

use anyhow::Context;

use mithril_common::StdResult;

/// Tool to check and remove unexpected files when downloading and unpacking Mithril archives
pub struct UnexpectedDownloadedFileVerifier {}

pub struct ExistingFilesBeforeDownloadUnpack {
    dir_to_check: PathBuf,
    list: Vec<PathBuf>,
}

impl UnexpectedDownloadedFileVerifier {
    /// `UnexpectedDownloadedFileVerifier` factory
    pub fn new() -> Self {
        Self {}
    }

    /// Return the list of existing files
    pub fn list_existing_file<P: AsRef<Path>>(
        &self,
        dir_to_check: P,
    ) -> StdResult<ExistingFilesBeforeDownloadUnpack> {
        let dir_to_check = dir_to_check.as_ref().to_path_buf();
        let mut existing_files: Vec<PathBuf> = std::fs::read_dir(&dir_to_check)
            .with_context(|| {
                format!(
                    "UnexpectedDownloadedFileVerifier failed: Failed to read directory {}",
                    dir_to_check.display()
                )
            })?
            .flat_map(|e| e.map(|e| e.path()))
            .collect();

        Ok(ExistingFilesBeforeDownloadUnpack {
            dir_to_check,
            list: existing_files,
        })
    }
}

impl ExistingFilesBeforeDownloadUnpack {
    /// Identify and delete unexpected files and folder
    ///
    /// Returns the name of the deleted items
    pub fn remove_unexpected_files(&self) -> StdResult<Option<Vec<String>>> {
        Ok(None)
    }

    // Note: ordering is slow and only matter for test
    #[cfg(test)]
    fn sorted_list(&self) -> Vec<PathBuf> {
        let mut list = self.list.clone();
        list.sort();
        list
    }
}

#[cfg(test)]
mod tests {
    use std::fs::{create_dir, File};
    use std::time::Instant;

    use mithril_common::digesters::immutable_trio_names;
    use mithril_common::temp_dir_create;

    use super::*;

    mod listing_existing_file {
        use super::*;

        #[test]
        fn when_dir_empty_return_empty_object() {
            let temp_dir = temp_dir_create!();
            let verifier = UnexpectedDownloadedFileVerifier::new();
            let existing_files = verifier.list_existing_file(&temp_dir).unwrap();

            assert_eq!(existing_files.list, Vec::<PathBuf>::new());
        }

        #[test]
        fn return_existing_files_and_dirs() {
            let temp_dir = temp_dir_create!();
            let verifier = UnexpectedDownloadedFileVerifier::new();
            create_dir(temp_dir.join("dir_1")).unwrap();
            create_dir(temp_dir.join("dir_2")).unwrap();
            File::create(temp_dir.join("file_1.txt")).unwrap();
            File::create(temp_dir.join("file_2.txt")).unwrap();
            File::create(temp_dir.join("dir_2").join("file_3.txt")).unwrap();

            let existing_files = verifier.list_existing_file(&temp_dir).unwrap();

            assert_eq!(
                existing_files.sorted_list(),
                vec![
                    temp_dir.join("dir_1"),
                    temp_dir.join("dir_2"),
                    temp_dir.join("file_1.txt"),
                    temp_dir.join("file_2.txt"),
                ]
            );
        }
    }

    // Note: this test does not have assertion, it's value is to measure time taken over a large
    // database to ensure that's doable without particular optimization
    #[test]
    fn checking_unexpected_file_against_a_large_immutable_directory() {
        let temp_dir = temp_dir_create!();
        let verifier = UnexpectedDownloadedFileVerifier::new();

        for immutable_file_name in (0..20000).flat_map(|i| immutable_trio_names(i)) {
            File::create(temp_dir.join(immutable_file_name)).unwrap();
        }

        let now = Instant::now();

        let _existing_files = verifier.list_existing_file(&temp_dir).unwrap();
        println!("elapsed time on list_existing_file: {:?}", now.elapsed());
    }
}
