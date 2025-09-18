use std::fs::File;
use std::io::prelude::Write;
use std::path::{Path, PathBuf};

use mithril_common::entities::{ImmutableFileNumber, SlotNumber};

use crate::entities::{ImmutableFile, LedgerStateSnapshot};
use crate::{IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR};

/// Dummy cardano db 'immutables' subdirectory content
struct DummyImmutableDb {
    /// The dummy cardano db 'immutables' directory path.
    dir: PathBuf,
    /// The [immutables files][ImmutableFile] in the dummy cardano db 'immutables' subdirectory.
    immutables_files: Vec<ImmutableFile>,
    /// Files that doesn't follow the immutable file name scheme in the dummy cardano db 'immutables' subdirectory.
    non_immutables_files: Vec<PathBuf>,
}

/// Dummy cardano db 'ledger' subdirectory content
struct DummyLedgerStateFolder {
    /// The dummy cardano db 'ledger' directory path.
    dir: PathBuf,
    /// The [ledger state snapshots][LedgerStateSnapshot] in the dummy cardano db 'ledger' subdirectory.
    ledger_state_snapshots: Vec<LedgerStateSnapshot>,
    /// Files that are not ledger snapshots in the dummy cardano db 'ledger' subdirectory.
    non_ledger_state_snapshots: Vec<PathBuf>,
}

impl DummyImmutableDb {
    /// Add an immutable chunk file and its primary & secondary to the dummy DB.
    pub fn add_immutable_file(&mut self) -> ImmutableFileNumber {
        let new_file_number = self.last_immutable_number().unwrap_or(0) + 1;
        let mut new_files = write_immutable_trio(None, &self.dir, new_file_number);

        self.immutables_files.append(&mut new_files);

        new_file_number
    }

    /// Return the file number of the last immutable
    pub fn last_immutable_number(&self) -> Option<ImmutableFileNumber> {
        self.immutables_files.last().map(|f| f.number)
    }
}

/// A dummy cardano db.
pub struct DummyCardanoDb {
    /// The dummy cardano db directory path.
    dir: PathBuf,

    /// Dummy immutable db
    immutable_db: DummyImmutableDb,

    /// Dummy ledger state folder
    ledger_state_folder: DummyLedgerStateFolder,
}

impl DummyCardanoDb {
    /// Return the cardano db directory path.
    pub fn get_dir(&self) -> &PathBuf {
        &self.dir
    }

    /// Return the immutable db directory path.
    pub fn get_immutable_dir(&self) -> &Path {
        &self.immutable_db.dir
    }

    /// Return the ledger directory path.
    pub fn get_ledger_dir(&self) -> &Path {
        &self.ledger_state_folder.dir
    }

    /// Return the volatile directory path.
    pub fn get_volatile_dir(&self) -> PathBuf {
        self.dir.join(VOLATILE_DIR)
    }

    /// Return the file number of the last immutable
    pub fn get_immutable_files(&self) -> &Vec<ImmutableFile> {
        &self.immutable_db.immutables_files
    }

    /// Return the path of the files in the ledger directory.
    pub fn get_ledger_state_snapshots(&self) -> &Vec<LedgerStateSnapshot> {
        &self.ledger_state_folder.ledger_state_snapshots
    }

    /// Return the non-ledger state snapshot files in the ledger directory
    pub fn get_non_ledger_state_snapshots(&self) -> &Vec<PathBuf> {
        &self.ledger_state_folder.non_ledger_state_snapshots
    }

    /// Add an immutable chunk file and its primary & secondary to the dummy DB.
    pub fn add_immutable_file(&mut self) -> ImmutableFileNumber {
        self.immutable_db.add_immutable_file()
    }

    /// Return the file number of the last immutable
    pub fn last_immutable_number(&self) -> Option<ImmutableFileNumber> {
        self.immutable_db.last_immutable_number()
    }

    /// Return the non-immutables files in the immutables directory
    pub fn get_non_immutables_files(&self) -> &Vec<PathBuf> {
        &self.immutable_db.non_immutables_files
    }
}

/// A [DummyCardanoDbBuilder] builder.
pub struct DummyCardanoDbBuilder {
    sub_dir: String,
    immutables_to_write: Vec<ImmutableFileNumber>,
    non_immutables_to_write: Vec<String>,
    append_uncompleted_trio: bool,
    immutable_file_size: Option<u64>,
    ledger_snapshot_in_memory_to_write: Vec<SlotNumber>,
    ledger_snapshot_legacy_to_write: Vec<SlotNumber>,
    non_ledger_snapshot_to_write: Vec<String>,
    ledger_file_size: Option<u64>,
    volatile_files_to_write: Vec<String>,
    volatile_file_size: Option<u64>,
}

impl DummyCardanoDbBuilder {
    /// [DummyCardanoDbBuilder] factory, will create a folder with the given `dirname` in the
    /// system temp directory, if it exists already it will be cleaned.
    pub fn new(dir_name: &str) -> Self {
        Self {
            sub_dir: dir_name.to_string(),
            immutables_to_write: vec![],
            non_immutables_to_write: vec![],
            append_uncompleted_trio: false,
            immutable_file_size: None,
            non_ledger_snapshot_to_write: vec![],
            ledger_snapshot_in_memory_to_write: vec![],
            ledger_snapshot_legacy_to_write: vec![],
            ledger_file_size: None,
            volatile_files_to_write: vec![],
            volatile_file_size: None,
        }
    }

    /// Set the immutables file number that will be used to generate the immutable files, for each
    /// number three files will be generated (a 'chunk', a 'primary' and a 'secondary' file).
    pub fn with_immutables(&mut self, immutables: &[ImmutableFileNumber]) -> &mut Self {
        self.immutables_to_write = immutables.to_vec();
        self
    }

    /// Set filenames to write to the db that doesn't follow the immutable file name scheme.
    pub fn with_non_immutables(&mut self, non_immutables: &[&str]) -> &mut Self {
        self.non_immutables_to_write = non_immutables.iter().map(|f| f.to_string()).collect();
        self
    }

    /// Set legacy ledger state snapshot slot numbers to write to the db in the 'ledger' subdirectory.
    pub fn with_legacy_ledger_snapshots(&mut self, snapshot_slot_numbers: &[u64]) -> &mut Self {
        self.ledger_snapshot_legacy_to_write =
            snapshot_slot_numbers.iter().map(|s| SlotNumber(*s)).collect();
        self
    }

    /// Set the slot numbers of utxo-hd in-memory snapshot folders to write to the db in the 'ledger' subdirectory.
    pub fn with_in_memory_ledger_snapshots(&mut self, snapshot_slot_numbers: &[u64]) -> &mut Self {
        self.ledger_snapshot_in_memory_to_write =
            snapshot_slot_numbers.iter().map(|s| SlotNumber(*s)).collect();
        self
    }

    /// Set filenames to write to the 'ledger' subdirectory that are not ledger snapshots.
    pub fn with_non_ledger_files(&mut self, files: &[&str]) -> &mut Self {
        self.non_ledger_snapshot_to_write = files.iter().map(|name| name.to_string()).collect();
        self
    }

    /// Set the size of all ledger files written by [build][Self::build] to the given `file_size` in bytes.
    ///
    /// Note: for types of ledger state snapshots that have more than one file, the size is evenly
    /// split across all files:
    /// * Legacy snapshots: not divided as it's a single file
    /// * In-memory snapshots: divided by three
    pub fn set_ledger_file_size(&mut self, file_size: u64) -> &mut Self {
        self.ledger_file_size = Some(file_size);
        self
    }

    /// Set volatile files to write to the db in the 'volatile' subdirectory.
    pub fn with_volatile_files(&mut self, files: &[&str]) -> &mut Self {
        self.volatile_files_to_write = files.iter().map(|f| f.to_string()).collect();
        self
    }

    /// Set the size of all volatile files written by [build][Self::build] to the given `file_size` in bytes.
    pub fn set_volatile_file_size(&mut self, file_size: u64) -> &mut Self {
        self.volatile_file_size = Some(file_size);
        self
    }

    /// Makes [build][Self::build] add another trio of immutables file, that won't be included
    /// in its returned vec, to simulate the last 3 'uncompleted / wip' files that can be found in
    /// a cardano immutable db.
    pub fn append_immutable_trio(&mut self) -> &mut Self {
        self.append_uncompleted_trio = true;
        self
    }

    /// Set the size of all immutable files written by [build][Self::build] to the given `file_size` in bytes.
    ///
    /// Note: by default, the size of the produced files is less than 1 kb.
    pub fn set_immutable_trio_file_size(&mut self, trio_file_size: u64) -> &mut Self {
        assert!(
            trio_file_size.is_multiple_of(3),
            "'trio_file_size' must be a multiple of 3"
        );

        self.immutable_file_size = Some(trio_file_size / 3);
        self
    }

    /// Build a [DummyCardanoDb].
    pub fn build(&self) -> DummyCardanoDb {
        let dir = get_test_dir(&self.sub_dir);

        let mut non_immutables_files = vec![];
        let mut ledger_state_snapshots = vec![];
        let mut non_ledger_state_snapshots = vec![];
        let mut immutable_numbers = self.immutables_to_write.clone();
        immutable_numbers.sort();

        if self.append_uncompleted_trio {
            write_immutable_trio(
                self.immutable_file_size,
                &dir.join(IMMUTABLE_DIR),
                match immutable_numbers.last() {
                    None => 0,
                    Some(last) => last + 1,
                },
            );
        }

        for non_immutable in &self.non_immutables_to_write {
            non_immutables_files.push(write_dummy_file(
                self.immutable_file_size,
                &dir.join(IMMUTABLE_DIR),
                non_immutable,
            ));
        }

        for filename in &self.non_ledger_snapshot_to_write {
            let ledger_file_path =
                write_dummy_file(self.ledger_file_size, &dir.join(LEDGER_DIR), filename);
            non_ledger_state_snapshots.push(ledger_file_path);
        }

        for slot_number in &self.ledger_snapshot_legacy_to_write {
            let ledger_file = write_dummy_file(
                self.ledger_file_size,
                &dir.join(LEDGER_DIR),
                &slot_number.to_string(),
            );

            ledger_state_snapshots.push(LedgerStateSnapshot::legacy(
                ledger_file,
                *slot_number,
                slot_number.to_string().into(),
            ));
        }

        for slot_number in &self.ledger_snapshot_in_memory_to_write {
            let ledger_state_snapshot =
                write_in_memory_ledger_snapshot(*slot_number, &dir, self.ledger_file_size);
            ledger_state_snapshots.push(ledger_state_snapshot);
        }

        for filename in &self.volatile_files_to_write {
            write_dummy_file(self.volatile_file_size, &dir.join(VOLATILE_DIR), filename);
        }

        let immutable_db = DummyImmutableDb {
            dir: dir.join(IMMUTABLE_DIR),
            immutables_files: immutable_numbers
                .into_iter()
                .flat_map(|ifn| {
                    write_immutable_trio(self.immutable_file_size, &dir.join(IMMUTABLE_DIR), ifn)
                })
                .collect::<Vec<_>>(),
            non_immutables_files,
        };

        let ledger_state_folder = DummyLedgerStateFolder {
            dir: dir.join(LEDGER_DIR),
            ledger_state_snapshots,
            non_ledger_state_snapshots,
        };

        DummyCardanoDb {
            dir,
            immutable_db,
            ledger_state_folder,
        }
    }
}

fn write_immutable_trio(
    optional_size: Option<u64>,
    dir: &Path,
    immutable: ImmutableFileNumber,
) -> Vec<ImmutableFile> {
    let mut result = vec![];
    for filename in [
        format!("{immutable:05}.chunk"),
        format!("{immutable:05}.primary"),
        format!("{immutable:05}.secondary"),
    ] {
        let file = write_dummy_file(optional_size, dir, &filename);
        result.push(ImmutableFile {
            number: immutable.to_owned(),
            path: file,
            filename: filename.to_string(),
        });
    }
    result
}

fn write_in_memory_ledger_snapshot(
    slot_number: SlotNumber,
    dir: &Path,
    optional_size: Option<u64>,
) -> LedgerStateSnapshot {
    let ledger_folder_name = slot_number.to_string();
    let ledger_folder_path = dir.join(LEDGER_DIR).join(&ledger_folder_name);
    let optional_file_size = optional_size.map(|s| s / 3);

    std::fs::create_dir_all(ledger_folder_path.join(LedgerStateSnapshot::IN_MEMORY_TABLES))
        .unwrap();
    write_dummy_file(
        optional_file_size,
        &ledger_folder_path,
        LedgerStateSnapshot::IN_MEMORY_STATE,
    );
    write_dummy_file(
        optional_file_size,
        &ledger_folder_path,
        LedgerStateSnapshot::IN_MEMORY_META,
    );
    write_dummy_file(
        optional_file_size,
        &ledger_folder_path.join(LedgerStateSnapshot::IN_MEMORY_TABLES),
        LedgerStateSnapshot::IN_MEMORY_TVAR,
    );

    LedgerStateSnapshot::in_memory(ledger_folder_path, slot_number, ledger_folder_name.into())
}

/// Create a file with the given name in the given dir, write some text to it, and then
/// return its path.
fn write_dummy_file(optional_size: Option<u64>, dir: &Path, filename: &str) -> PathBuf {
    let file = dir.join(Path::new(filename));
    let mut source_file = File::create(&file).unwrap();

    write!(source_file, "This is a test file named '{filename}'").unwrap();

    if let Some(file_size) = optional_size {
        writeln!(source_file).unwrap();
        source_file.set_len(file_size).unwrap();
    }

    file
}

fn get_test_dir(subdir_name: &str) -> PathBuf {
    // Note: used the common `TestDir` api before, but as the DummyCardanoDb is public, `test_tools` can't be used.
    let db_dir = std::env::temp_dir()
        .join("mithril_test")
        .join("test_cardano_db")
        .join(subdir_name);

    if db_dir.exists() {
        std::fs::remove_dir_all(&db_dir)
            .unwrap_or_else(|e| panic!("Could not remove dir {db_dir:?}: {e}"));
    }
    std::fs::create_dir_all(&db_dir)
        .unwrap_or_else(|e| panic!("Could not create dir {db_dir:?}: {e}"));

    for subdir_name in [LEDGER_DIR, IMMUTABLE_DIR, VOLATILE_DIR] {
        std::fs::create_dir(db_dir.join(subdir_name)).unwrap();
    }

    db_dir
}

#[cfg(test)]
mod tests {
    use mithril_common::{assert_dir_eq, current_function};

    use super::*;

    #[test]
    fn writing_empty_dummy_cardano_db_structure() {
        let db = DummyCardanoDbBuilder::new(current_function!()).build();

        assert_eq!(db.get_immutable_dir(), db.get_dir().join(IMMUTABLE_DIR));
        assert_eq!(db.get_ledger_dir(), db.get_dir().join(LEDGER_DIR));
        assert_eq!(db.get_volatile_dir(), db.get_dir().join(VOLATILE_DIR));
        assert_eq!(db.last_immutable_number(), None);
        assert_dir_eq!(
            &db.dir,
            format!(
                "* {IMMUTABLE_DIR}/
                 * {LEDGER_DIR}/
                 * {VOLATILE_DIR}/"
            )
        );
    }

    #[test]
    fn writing_immutable_files() {
        let db = DummyCardanoDbBuilder::new(current_function!())
            .with_immutables(&[1, 2])
            .build();

        assert_eq!(db.last_immutable_number(), Some(2));
        assert_eq!(db.get_immutable_files().len(), 6); // Each immutable is a trio of files
        assert_dir_eq!(
            &db.dir,
            format!(
                "* {IMMUTABLE_DIR}/
                 ** 00001.chunk
                 ** 00001.primary
                 ** 00001.secondary
                 ** 00002.chunk
                 ** 00002.primary
                 ** 00002.secondary
                 * {LEDGER_DIR}/
                 * {VOLATILE_DIR}/"
            )
        );
    }

    #[test]
    fn adding_non_completed_immutable_files_trio_is_not_take_in_account_in_resulting_db_metadata() {
        let db = DummyCardanoDbBuilder::new(current_function!())
            .with_immutables(&[1])
            .append_immutable_trio()
            .build();

        assert_eq!(db.last_immutable_number(), Some(1));
        assert_eq!(db.get_immutable_files().len(), 3); // Each immutable is a trio of files
        assert_dir_eq!(
            &db.dir,
            format!(
                "* {IMMUTABLE_DIR}/
                 ** 00001.chunk
                 ** 00001.primary
                 ** 00001.secondary
                 ** 00002.chunk
                 ** 00002.primary
                 ** 00002.secondary
                 * {LEDGER_DIR}/
                 * {VOLATILE_DIR}/"
            )
        );
    }

    #[test]
    fn writing_non_immutable_files() {
        let db = DummyCardanoDbBuilder::new(current_function!())
            .with_non_immutables(&["test1.txt", "test2.txt"])
            .build();

        let non_immutable_files = db.get_non_immutables_files();
        assert_eq!(
            non_immutable_files,
            &vec![
                db.get_dir().join(IMMUTABLE_DIR).join("test1.txt"),
                db.get_dir().join(IMMUTABLE_DIR).join("test2.txt"),
            ]
        );
        assert_dir_eq!(
            &db.dir,
            format!(
                "* {IMMUTABLE_DIR}/
                 ** test1.txt
                 ** test2.txt
                 * {LEDGER_DIR}/
                 * {VOLATILE_DIR}/"
            )
        );
    }

    #[test]
    fn writing_ledger_snapshots() {
        let db = DummyCardanoDbBuilder::new(current_function!())
            .with_legacy_ledger_snapshots(&[100])
            .with_in_memory_ledger_snapshots(&[200])
            .build();

        let snapshots = db.get_ledger_state_snapshots();
        assert_eq!(snapshots.len(), 2);
        assert!(snapshots.iter().any(|s| s.slot_number() == SlotNumber(100)));
        assert!(snapshots.iter().any(|s| s.slot_number() == SlotNumber(200)));
        assert_dir_eq!(
            &db.dir,
            format!(
                "* {IMMUTABLE_DIR}/
                 * {LEDGER_DIR}/
                 ** 200/
                 *** {}/
                 **** {}
                 *** {}
                 *** {}
                 ** 100
                 * {VOLATILE_DIR}/",
                LedgerStateSnapshot::IN_MEMORY_TABLES,
                LedgerStateSnapshot::IN_MEMORY_TVAR,
                LedgerStateSnapshot::IN_MEMORY_META,
                LedgerStateSnapshot::IN_MEMORY_STATE,
            )
        );
    }

    #[test]
    fn setting_file_sizes() {
        fn get_file_size(path: &Path) -> u64 {
            std::fs::metadata(path).unwrap().len()
        }

        let (immutable_file_size, ledger_file_size, volatile_file_size) = (3000, 6000, 9000);
        let db = DummyCardanoDbBuilder::new(current_function!())
            .with_immutables(&[1])
            .with_legacy_ledger_snapshots(&[100])
            .with_in_memory_ledger_snapshots(&[200])
            .with_volatile_files(&["test.txt"])
            .set_immutable_trio_file_size(immutable_file_size)
            .set_ledger_file_size(ledger_file_size)
            .set_volatile_file_size(volatile_file_size)
            .build();

        for file in db.get_immutable_files() {
            assert_eq!(get_file_size(&file.path), immutable_file_size / 3);
        }

        for ledger_state_snapshot in db.get_ledger_state_snapshots() {
            match ledger_state_snapshot {
                LedgerStateSnapshot::Legacy { path, .. } => {
                    assert_eq!(get_file_size(path), ledger_file_size);
                }
                LedgerStateSnapshot::InMemory { path, .. } => {
                    assert_eq!(
                        get_file_size(&path.join(LedgerStateSnapshot::IN_MEMORY_STATE)),
                        ledger_file_size / 3
                    );
                    assert_eq!(
                        get_file_size(&path.join(LedgerStateSnapshot::IN_MEMORY_META)),
                        ledger_file_size / 3
                    );
                    assert_eq!(
                        get_file_size(
                            &path
                                .join(LedgerStateSnapshot::IN_MEMORY_TABLES)
                                .join(LedgerStateSnapshot::IN_MEMORY_TVAR)
                        ),
                        ledger_file_size / 3
                    );
                }
            }
        }

        assert_eq!(
            get_file_size(&db.get_volatile_dir().join("test.txt")),
            volatile_file_size
        );
    }
}
