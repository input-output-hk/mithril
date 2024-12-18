use crate::test_utils::TempDir;
use crate::{digesters::ImmutableFile, entities::ImmutableFileNumber};
use std::{
    fs::File,
    io::prelude::Write,
    path::{Path, PathBuf},
};

/// Directory name for the immutable files.
pub const IMMUTABLE_DIR: &str = "immutable";
/// Directory name for the ledger files.
pub const LEDGER_DIR: &str = "ledger";
/// Directory name for the volatile files.
pub const VOLATILE_DIR: &str = "volatile";

/// A dummy cardano immutable db.
struct DummyImmutableDb {
    /// The dummy cardano db directory path.
    dir: PathBuf,
    /// The [immutables files][ImmutableFile] in the dummy cardano db.
    immutables_files: Vec<ImmutableFile>,
    /// Files that doesn't follow the immutable file name scheme in the dummy cardano db.
    non_immutables_files: Vec<PathBuf>,
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
}

impl DummyCardanoDb {
    /// Return the cardano db directory path.
    pub fn get_dir(&self) -> &PathBuf {
        &self.dir
    }

    /// Return the immutable db directory path.
    pub fn get_immutable_dir(&self) -> &PathBuf {
        &self.immutable_db.dir
    }

    /// Return the file number of the last immutable
    pub fn get_immutable_files(&self) -> &Vec<ImmutableFile> {
        &self.immutable_db.immutables_files
    }

    /// Add an immutable chunk file and its primary & secondary to the dummy DB.
    pub fn add_immutable_file(&mut self) -> ImmutableFileNumber {
        self.immutable_db.add_immutable_file()
    }

    /// Return the file number of the last immutable
    pub fn last_immutable_number(&self) -> Option<ImmutableFileNumber> {
        self.immutable_db.last_immutable_number()
    }

    /// Return the non immutables files in the immutables directory
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
    ledger_files_to_write: Vec<String>,
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
            ledger_files_to_write: vec![],
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

    /// Set ledger files to write to the db in the 'ledger' subdirectory.
    pub fn with_ledger_files(&mut self, files: &[&str]) -> &mut Self {
        self.ledger_files_to_write = files.iter().map(|name| name.to_string()).collect();
        self
    }

    /// Set the size of all ledger files written by [build][Self::build] to the given `file_size` in bytes.
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
    /// Note: by default the size of the produced files is less than a 1kb.
    pub fn set_immutable_trio_file_size(&mut self, trio_file_size: u64) -> &mut Self {
        assert!(
            trio_file_size % 3 == 0,
            "'trio_file_size' must be a multiple of 3"
        );

        self.immutable_file_size = Some(trio_file_size / 3);
        self
    }

    /// Build a [DummyCardanoDb].
    pub fn build(&self) -> DummyCardanoDb {
        let dir = get_test_dir(&self.sub_dir);

        let mut non_immutables_files = vec![];
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

        for filename in &self.ledger_files_to_write {
            write_dummy_file(self.ledger_file_size, &dir.join(LEDGER_DIR), filename);
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

        DummyCardanoDb { dir, immutable_db }
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
    let db_dir = TempDir::create("test_cardano_db", subdir_name);
    for subdir_name in [LEDGER_DIR, IMMUTABLE_DIR, VOLATILE_DIR] {
        std::fs::create_dir(db_dir.join(subdir_name)).unwrap();
    }

    db_dir
}
