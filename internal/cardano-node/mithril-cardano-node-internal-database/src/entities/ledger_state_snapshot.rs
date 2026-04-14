use std::{
    cmp::Ordering,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};
use thiserror::Error;
use walkdir::WalkDir;

use mithril_common::{StdResult, entities::SlotNumber};

use crate::LEDGER_DIR;

/// Walk the given path and return the first directory named "ledger" it finds
fn find_ledger_dir(path_to_walk: &Path) -> Option<PathBuf> {
    WalkDir::new(path_to_walk)
        .into_iter()
        .filter_entry(|e| e.file_type().is_dir())
        .filter_map(|e| e.ok())
        .find(|f| f.file_name() == LEDGER_DIR)
        .map(|e| e.into_path())
}

/// Returns true if the `meta` file in the given path exists and contains a JSON object
/// with the `backend` field set to `"utxohd-mem"`.
fn has_in_memory_meta_backend(path: &Path) -> StdResult<bool> {
    let meta_content = fs::read_to_string(path.join(LedgerStateSnapshot::IN_MEMORY_META))?;
    let meta_json = serde_json::from_str::<serde_json::Value>(&meta_content)?;

    Ok(meta_json.get("backend").and_then(|v| v.as_str()) == Some("utxohd-mem"))
}

fn is_ledger_state_snapshot(path: &Path) -> bool {
    if path.is_dir() {
        has_in_memory_meta_backend(path).unwrap_or(false)
            && path.join(LedgerStateSnapshot::IN_MEMORY_STATE).is_file()
            && (path
                .join(LedgerStateSnapshot::IN_MEMORY_TABLES)
                .join(LedgerStateSnapshot::IN_MEMORY_TVAR)
                .is_file()
                || path.join(LedgerStateSnapshot::IN_MEMORY_TABLES).is_file())
    } else {
        path.is_file()
    }
}

/// Represent a ledger file in a Cardano node database directory
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
    /// Snapshot of a UTxO-HD in-memory ledger state (Cardano node up to 10.6)
    InMemoryUpTo10_6 {
        /// The path to the ledger file
        path: PathBuf,
        /// The ledger file slot number
        slot_number: SlotNumber,
        /// Name of the ledger state folder
        folder_name: OsString,
    },
    /// Snapshot of a UTxO-HD in-memory ledger state (Cardano node 10.7 and above)
    InMemoryFrom10_7 {
        /// The path to the ledger file
        path: PathBuf,
        /// The ledger file slot number
        slot_number: SlotNumber,
        /// Name of the ledger state folder
        folder_name: OsString,
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
    /// Filename of the in-memory ledger snapshot 'meta' file
    pub const IN_MEMORY_META: &'static str = "meta";
    /// Filename of the in-memory ledger snapshot 'state' file
    pub const IN_MEMORY_STATE: &'static str = "state";
    /// Directory name of the in-memory ledger snapshot 'tables' folder
    pub const IN_MEMORY_TABLES: &'static str = "tables";
    /// Filename of the in-memory ledger snapshot 'tables/tvar' file
    pub const IN_MEMORY_TVAR: &'static str = "tvar";

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
                    if path.is_dir() {
                        let tables_path = path.join(Self::IN_MEMORY_TABLES);
                        if tables_path.is_file() {
                            Self::InMemoryFrom10_7 {
                                path: path.to_path_buf(),
                                slot_number: SlotNumber(number),
                                folder_name: filename.to_os_string(),
                            }
                        } else {
                            Self::InMemoryUpTo10_6 {
                                path: path.to_path_buf(),
                                slot_number: SlotNumber(number),
                                folder_name: filename.to_os_string(),
                            }
                        }
                    } else {
                        Self::legacy(
                            path.to_path_buf(),
                            SlotNumber(number),
                            filename.to_os_string(),
                        )
                    }
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
            .filter_entry(|e| is_ledger_state_snapshot(e.path()))
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
    /// Returned paths are relative to the cardano node database ledger dir
    pub fn get_files_relative_path(&self) -> Vec<PathBuf> {
        match self {
            LedgerStateSnapshot::Legacy { filename, .. } => {
                vec![PathBuf::from(filename)]
            }
            LedgerStateSnapshot::InMemoryUpTo10_6 { folder_name, .. } => {
                vec![
                    PathBuf::from(folder_name).join(Self::IN_MEMORY_META),
                    PathBuf::from(folder_name).join(Self::IN_MEMORY_STATE),
                    PathBuf::from(folder_name)
                        .join(Self::IN_MEMORY_TABLES)
                        .join(Self::IN_MEMORY_TVAR),
                ]
            }
            LedgerStateSnapshot::InMemoryFrom10_7 { folder_name, .. } => {
                vec![
                    PathBuf::from(folder_name).join(Self::IN_MEMORY_META),
                    PathBuf::from(folder_name).join(Self::IN_MEMORY_STATE),
                    PathBuf::from(folder_name).join(Self::IN_MEMORY_TABLES),
                ]
            }
        }
    }

    /// Return the slot number when this snapshot was taken
    pub fn slot_number(&self) -> SlotNumber {
        match self {
            LedgerStateSnapshot::Legacy { slot_number, .. }
            | LedgerStateSnapshot::InMemoryUpTo10_6 { slot_number, .. }
            | LedgerStateSnapshot::InMemoryFrom10_7 { slot_number, .. } => *slot_number,
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
    use std::fs::{File, create_dir};
    use std::io::prelude::*;

    use mithril_common::test::temp_dir_create;

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

    fn create_in_memory_meta_file(dir: &Path) {
        let mut f = File::create(dir.join(LedgerStateSnapshot::IN_MEMORY_META)).unwrap();
        write!(f, r#"{{"backend": "utxohd-mem"}}"#).unwrap();
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

    // UTxO-HD in-memory rules:
    // - a folder named after the slot number at which the snapshots are taken (same naming convention as
    // legacy state snapshot)
    // - contains three files, with one in a subfolder:
    //   - "/meta"
    //   - "/state"
    //   - "/tables/tvar" (Cardano node up to 10.6) or "/tables" as a file (Cardano node 10.7+)
    mod utxo_hd_in_memory_ledger_state {
        use std::fs::create_dir_all;

        use super::*;

        #[test]
        fn list_all_ledger_state_should_not_include_utxo_hd_folder_that_does_not_contains_meta_state_or_tvar_files()
         {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);

            let ledger_empty_dir = ledger_dir.join("000");
            create_dir(&ledger_empty_dir).unwrap();

            let ledger_with_missing_meta_files = ledger_dir.join("100");
            create_dir_all(
                ledger_with_missing_meta_files.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
            )
            .unwrap();
            create_fake_files(
                &ledger_with_missing_meta_files,
                &[LedgerStateSnapshot::IN_MEMORY_STATE],
            );
            create_fake_files(
                &ledger_with_missing_meta_files.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
                &[LedgerStateSnapshot::IN_MEMORY_TVAR],
            );

            let ledger_with_missing_state_files = ledger_dir.join("200");
            create_dir_all(
                ledger_with_missing_state_files.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
            )
            .unwrap();
            create_in_memory_meta_file(&ledger_with_missing_state_files);
            create_fake_files(
                &ledger_with_missing_state_files.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
                &[LedgerStateSnapshot::IN_MEMORY_TVAR],
            );

            let ledger_with_missing_tvar_files = ledger_dir.join("300");
            create_dir_all(
                ledger_with_missing_tvar_files.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
            )
            .unwrap();
            create_in_memory_meta_file(&ledger_with_missing_tvar_files);
            create_fake_files(
                &ledger_with_missing_tvar_files,
                &[LedgerStateSnapshot::IN_MEMORY_STATE],
            );

            let ledger_with_missing_table_folder = ledger_dir.join("400");
            create_dir(&ledger_with_missing_table_folder).unwrap();
            create_in_memory_meta_file(&ledger_with_missing_table_folder);
            create_fake_files(
                &ledger_with_missing_table_folder,
                &[LedgerStateSnapshot::IN_MEMORY_STATE],
            );

            let result = LedgerStateSnapshot::list_all_in_dir(&target_dir).unwrap();

            assert_eq!(Vec::<LedgerStateSnapshot>::new(), result);
        }

        #[test]
        fn list_all_ledger_state_should_not_include_utxo_hd_folder_with_wrong_meta_backend() {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);

            let ledger_with_wrong_backend = ledger_dir.join("100");
            create_dir_all(ledger_with_wrong_backend.join(LedgerStateSnapshot::IN_MEMORY_TABLES))
                .unwrap();
            let mut meta =
                File::create(ledger_with_wrong_backend.join(LedgerStateSnapshot::IN_MEMORY_META))
                    .unwrap();
            write!(meta, r#"{{"backend": "utxohd-lmdb"}}"#).unwrap();
            create_fake_files(
                &ledger_with_wrong_backend,
                &[LedgerStateSnapshot::IN_MEMORY_STATE],
            );
            create_fake_files(
                &ledger_with_wrong_backend.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
                &[LedgerStateSnapshot::IN_MEMORY_TVAR],
            );

            let result = LedgerStateSnapshot::list_all_in_dir(&target_dir).unwrap();

            assert_eq!(Vec::<LedgerStateSnapshot>::new(), result);
        }

        #[test]
        fn list_all_ledger_state_should_not_include_utxo_hd_folder_with_non_json_meta() {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);

            let ledger_with_non_json_meta = ledger_dir.join("200");
            create_dir_all(ledger_with_non_json_meta.join(LedgerStateSnapshot::IN_MEMORY_TABLES))
                .unwrap();
            create_fake_files(
                &ledger_with_non_json_meta,
                &[
                    LedgerStateSnapshot::IN_MEMORY_META,
                    LedgerStateSnapshot::IN_MEMORY_STATE,
                ],
            );
            create_fake_files(
                &ledger_with_non_json_meta.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
                &[LedgerStateSnapshot::IN_MEMORY_TVAR],
            );

            let result = LedgerStateSnapshot::list_all_in_dir(&target_dir).unwrap();

            assert!(result.is_empty());
        }

        #[test]
        fn list_all_ledger_state_with_valid_up_to_10_6_folder_structure() {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);

            let ledger_state = ledger_dir.join("200");
            create_dir_all(ledger_state.join(LedgerStateSnapshot::IN_MEMORY_TABLES)).unwrap();
            create_in_memory_meta_file(&ledger_state);
            create_fake_files(&ledger_state, &[LedgerStateSnapshot::IN_MEMORY_STATE]);
            create_fake_files(
                &ledger_state.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
                &[LedgerStateSnapshot::IN_MEMORY_TVAR],
            );

            let result = LedgerStateSnapshot::list_all_in_dir(&target_dir).unwrap();

            assert_eq!(
                vec![LedgerStateSnapshot::InMemoryUpTo10_6 {
                    path: ledger_state,
                    slot_number: SlotNumber(200),
                    folder_name: "200".into(),
                }],
                result
            );
        }

        #[test]
        fn list_all_ledger_state_with_valid_from_10_7_folder_structure() {
            let target_dir = temp_dir_create!();
            let ledger_dir = create_ledger_dir(&target_dir);

            let ledger_state = ledger_dir.join("200");
            create_dir(&ledger_state).unwrap();
            create_in_memory_meta_file(&ledger_state);
            create_fake_files(
                &ledger_state,
                &[
                    LedgerStateSnapshot::IN_MEMORY_STATE,
                    LedgerStateSnapshot::IN_MEMORY_TABLES,
                ],
            );

            let result = LedgerStateSnapshot::list_all_in_dir(&target_dir).unwrap();

            assert_eq!(
                vec![LedgerStateSnapshot::InMemoryFrom10_7 {
                    path: ledger_state,
                    slot_number: SlotNumber(200),
                    folder_name: "200".into(),
                }],
                result
            );
        }

        #[test]
        fn get_relative_path_for_up_to_10_6_lists_meta_state_and_tvar() {
            let ledger_state = LedgerStateSnapshot::InMemoryUpTo10_6 {
                path: PathBuf::from("/tmp/050"),
                slot_number: SlotNumber(50),
                folder_name: "050".into(),
            };

            assert_eq!(
                vec![
                    PathBuf::from("050").join(LedgerStateSnapshot::IN_MEMORY_META),
                    PathBuf::from("050").join(LedgerStateSnapshot::IN_MEMORY_STATE),
                    PathBuf::from("050")
                        .join(LedgerStateSnapshot::IN_MEMORY_TABLES)
                        .join(LedgerStateSnapshot::IN_MEMORY_TVAR)
                ],
                ledger_state.get_files_relative_path(),
            )
        }

        #[test]
        fn get_relative_path_for_from_10_7_lists_meta_state_and_tables_file() {
            let ledger_state = LedgerStateSnapshot::InMemoryFrom10_7 {
                path: PathBuf::from("/tmp/050"),
                slot_number: SlotNumber(50),
                folder_name: "050".into(),
            };

            assert_eq!(
                vec![
                    PathBuf::from("050").join(LedgerStateSnapshot::IN_MEMORY_META),
                    PathBuf::from("050").join(LedgerStateSnapshot::IN_MEMORY_STATE),
                    PathBuf::from("050").join(LedgerStateSnapshot::IN_MEMORY_TABLES),
                ],
                ledger_state.get_files_relative_path(),
            )
        }
    }
}
