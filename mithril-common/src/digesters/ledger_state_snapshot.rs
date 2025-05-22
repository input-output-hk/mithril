use std::{
    cmp::Ordering,
    ffi::OsString,
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
pub enum LedgerStateSnapshot {
    /// Snapshot of a legacy ledger state (before UTxO-HD)
    Legacy {
        /// The path to the ledger file
        path: PathBuf,
        /// The ledger file slot number
        slot_number: SlotNumber,
        /// The filename
        filename: OsString,
    },
}

/// [LedgerStateSnapshot::list_all_in_dir] related errors.
#[derive(Error, Debug)]
pub enum LedgerStateSnapshotListingError {
    /// Raised when the "ledger" folder could not be found in a file structure.
    #[error("Couldn't find the 'ledger' folder in '{0:?}'")]
    MissingLedgerFolder(PathBuf),
}

impl LedgerStateSnapshot {
    /// `LedgerStateSnapshot::Legacy` factory
    pub fn legacy(path: PathBuf, slot_number: SlotNumber, filename: OsString) -> Self {
        Self::Legacy {
            path,
            slot_number,
            filename,
        }
    }

    /// Convert a path to a [LedgerStateSnapshot] if it satisfies the constraints.
    ///
    /// The constraints are:
    /// - legacy state snapshot: the path must be a file, the filename should only contain a number (no
    ///   extension).
    pub fn from_path(path: &Path) -> Option<LedgerStateSnapshot> {
        path.file_name().and_then(|filename| {
            filename
                .to_string_lossy()
                .parse::<u64>()
                .map(|number| {
                    Self::legacy(
                        path.to_path_buf(),
                        SlotNumber(number),
                        filename.to_os_string(),
                    )
                })
                .ok()
        })
    }

    /// List all [`LedgerStateSnapshot`] in a given directory.
    pub fn list_all_in_dir(
        dir: &Path,
    ) -> Result<Vec<LedgerStateSnapshot>, LedgerStateSnapshotListingError> {
        let ledger_dir = find_ledger_dir(dir).ok_or(
            LedgerStateSnapshotListingError::MissingLedgerFolder(dir.to_path_buf()),
        )?;
        let mut files: Vec<LedgerStateSnapshot> = vec![];

        for path in WalkDir::new(ledger_dir)
            .min_depth(1)
            .max_depth(1)
            .into_iter()
            .filter_entry(|e| e.file_type().is_file())
            .filter_map(|file| file.ok())
        {
            if let Some(ledger_file) = LedgerStateSnapshot::from_path(path.path()) {
                files.push(ledger_file);
            }
        }
        files.sort();

        Ok(files)
    }

    /// Return paths to all files that constitute this snapshot
    ///
    /// Returned path are relative to the cardano node database ledger dir
    pub fn get_files_relative_path(&self) -> Vec<PathBuf> {
        match self {
            LedgerStateSnapshot::Legacy { filename, .. } => vec![PathBuf::from(filename)],
        }
    }

    /// Return the slot number when this snapshot was taken
    pub fn slot_number(&self) -> SlotNumber {
        match self {
            LedgerStateSnapshot::Legacy { slot_number, .. } => *slot_number,
        }
    }
}

impl PartialOrd for LedgerStateSnapshot {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LedgerStateSnapshot {
    fn cmp(&self, other: &Self) -> Ordering {
        self.slot_number().cmp(&other.slot_number())
    }
}

#[cfg(test)]
mod tests {
    use std::fs::{create_dir, File};
    use std::io::prelude::*;

    use crate::test_utils::temp_dir_create;

    use super::*;

    fn create_ledger_dir(parent_dir: &Path) -> PathBuf {
        let ledger_dir = parent_dir.join(LEDGER_DIR);
        create_dir(&ledger_dir).unwrap();
        ledger_dir
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(file).unwrap();
            write!(source_file, "This is a test file named '{filename}'").unwrap();
        }
    }

    fn extract_filenames(ledger_files: &[LedgerStateSnapshot]) -> Vec<String> {
        ledger_files
            .iter()
            .flat_map(|i| i.get_files_relative_path())
            .map(|p| p.file_name().unwrap().to_string_lossy().to_string())
            .collect()
    }

    #[test]
    fn list_all_ledger_file_fail_if_not_in_ledger_dir() {
        let target_dir = temp_dir_create!();

        LedgerStateSnapshot::list_all_in_dir(&target_dir)
            .expect_err("LedgerStateSnapshot::list_all_in_dir should have Failed");
    }

    #[test]
    fn list_all_ledger_file_should_works_in_a_empty_folder() {
        let target_dir = temp_dir_create!();
        create_ledger_dir(&target_dir);
        let result = LedgerStateSnapshot::list_all_in_dir(&target_dir)
            .expect("LedgerStateSnapshot::list_all_in_dir should work in a empty folder");

        assert_eq!(Vec::<LedgerStateSnapshot>::new(), result);
    }

    mod legacy_ledger_state {
        use super::*;

        #[test]
        fn list_all_ledger_file_order_should_be_deterministic() {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);
            let entries = vec!["424", "123", "124", "00125", "21", "223", "0423"];
            create_fake_files(&ledger_dir, &entries);
            let ledger_files = LedgerStateSnapshot::list_all_in_dir(&target_dir)
                .expect("LedgerStateSnapshot::list_all_in_dir Failed");

            assert_eq!(
                vec!["21", "123", "124", "00125", "223", "0423", "424"],
                extract_filenames(&ledger_files)
            );
        }

        #[test]
        fn list_all_ledger_file_should_work_with_non_ledger_files() {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);
            let entries = vec!["123", "124", "README.md", "124.back"];
            create_fake_files(&ledger_dir, &entries);
            let ledger_files = LedgerStateSnapshot::list_all_in_dir(&target_dir)
                .expect("LedgerStateSnapshot::list_all_in_dir Failed");

            assert_eq!(vec!["123", "124"], extract_filenames(&ledger_files));
        }
    }
}
