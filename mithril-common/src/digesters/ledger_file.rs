use std::{
    cmp::Ordering,
    path::{Path, PathBuf},
};
use thiserror::Error;
use walkdir::WalkDir;

use crate::digesters::LEDGER_DIR;
use crate::entities::SlotNumber;

/// Walk the given path and return the first directory named "ledger" it finds
fn find_ledger_dir(path_to_walk: &Path) -> Option<PathBuf> {
    WalkDir::new(path_to_walk)
        .into_iter()
        .filter_entry(|e| e.file_type().is_dir())
        .filter_map(|e| e.ok())
        .find(|f| f.file_name() == LEDGER_DIR)
        .map(|e| e.into_path())
}

/// Represent an ledger file in a Cardano node database directory
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LedgerFile {
    /// The path to the ledger file
    pub path: PathBuf,

    /// The ledger file slot number
    pub slot_number: SlotNumber,

    /// The filename
    pub filename: String,
}

/// [LedgerFile::list_all_in_dir] related errors.
#[derive(Error, Debug)]
pub enum LedgerFileListingError {
    /// Raised when the "ledger" folder could not be found in a file structure.
    #[error("Couldn't find the 'ledger' folder in '{0:?}'")]
    MissingLedgerFolder(PathBuf),
}

impl LedgerFile {
    /// LedgerFile factory
    pub fn new<T: Into<String>>(path: PathBuf, slot_number: SlotNumber, filename: T) -> Self {
        Self {
            path,
            slot_number,
            filename: filename.into(),
        }
    }

    /// Convert a path to a LedgerFile if it satisfies the LedgerFile constraints.
    ///
    /// The constraints are: the path must be a file, the filename should only contain a number (no
    /// extension).
    pub fn from_path(path: &Path) -> Option<LedgerFile> {
        path.file_name()
            .map(|name| name.to_string_lossy())
            .and_then(|filename| {
                filename
                    .parse::<u64>()
                    .map(|number| Self::new(path.to_path_buf(), SlotNumber(number), filename))
                    .ok()
            })
    }

    /// List all [`LedgerFile`] in a given directory.
    pub fn list_all_in_dir(dir: &Path) -> Result<Vec<LedgerFile>, LedgerFileListingError> {
        let ledger_dir = find_ledger_dir(dir).ok_or(
            LedgerFileListingError::MissingLedgerFolder(dir.to_path_buf()),
        )?;
        let mut files: Vec<LedgerFile> = vec![];

        for path in WalkDir::new(ledger_dir)
            .min_depth(1)
            .max_depth(1)
            .into_iter()
            .filter_entry(|e| e.file_type().is_file())
            .filter_map(|file| file.ok())
        {
            if let Some(ledger_file) = LedgerFile::from_path(path.path()) {
                files.push(ledger_file);
            }
        }
        files.sort();

        Ok(files)
    }
}

impl PartialOrd for LedgerFile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LedgerFile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.slot_number
            .cmp(&other.slot_number)
            .then(self.path.cmp(&other.path))
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::{Path, PathBuf};

    use crate::test_utils::TempDir;

    use super::LedgerFile;

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        TempDir::create("ledger_file", subdir_name)
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(file).unwrap();
            write!(source_file, "This is a test file named '{filename}'").unwrap();
        }
    }

    fn extract_filenames(ledger_files: &[LedgerFile]) -> Vec<String> {
        ledger_files
            .iter()
            .map(|i| i.path.file_name().unwrap().to_str().unwrap().to_owned())
            .collect()
    }

    #[test]
    fn list_all_ledger_file_fail_if_not_in_ledger_dir() {
        let target_dir = get_test_dir("list_all_ledger_file_fail_if_not_in_ledger_dir/invalid");
        let entries = vec![];
        create_fake_files(&target_dir, &entries);

        LedgerFile::list_all_in_dir(target_dir.parent().unwrap())
            .expect_err("LedgerFile::list_all_in_dir should have Failed");
    }

    #[test]
    fn list_all_ledger_file_should_works_in_a_empty_folder() {
        let target_dir = get_test_dir("list_all_ledger_file_should_works_in_a_empty_folder/ledger");
        let result = LedgerFile::list_all_in_dir(target_dir.parent().unwrap())
            .expect("LedgerFile::list_all_in_dir should work in a empty folder");

        assert!(result.is_empty());
    }

    #[test]
    fn list_all_ledger_file_order_should_be_deterministic() {
        let target_dir = get_test_dir("list_all_ledger_file_order_should_be_deterministic/ledger");
        let entries = vec!["424", "123", "124", "00125", "21", "223", "0423"];
        create_fake_files(&target_dir, &entries);
        let ledger_files = LedgerFile::list_all_in_dir(target_dir.parent().unwrap())
            .expect("LedgerFile::list_all_in_dir Failed");

        assert_eq!(
            vec!["21", "123", "124", "00125", "223", "0423", "424"],
            extract_filenames(&ledger_files)
        );
    }

    #[test]
    fn list_all_ledger_file_should_work_with_non_ledger_files() {
        let target_dir =
            get_test_dir("list_all_ledger_file_should_work_with_non_ledger_files/ledger");
        let entries = vec!["123", "124", "README.md", "124.back"];
        create_fake_files(&target_dir, &entries);
        let ledger_files = LedgerFile::list_all_in_dir(target_dir.parent().unwrap())
            .expect("LedgerFile::list_all_in_dir Failed");

        assert_eq!(vec!["123", "124"], extract_filenames(&ledger_files));
    }
}
