use crate::{digesters::ImmutableFile, entities::ImmutableFileNumber};
use std::{
    fs::File,
    io::prelude::Write,
    path::{Path, PathBuf},
};

/// A [DummyImmutableDb] builder.
pub struct DummyImmutablesDbBuilder {
    dir: PathBuf,
    immutables_to_write: Vec<ImmutableFileNumber>,
    non_immutables_to_write: Vec<String>,
    append_uncompleted_trio: bool,
    file_size: Option<u64>,
}

/// A dummy cardano immutable db.
pub struct DummyImmutableDb {
    /// The dummy cardano db directory path.
    pub dir: PathBuf,
    /// The [immutables files][ImmutableFile] in the dummy cardano db.
    pub immutables_files: Vec<ImmutableFile>,
    /// Files that doesn't follow the immutable file name scheme in the dummy cardano db.
    pub non_immutables_files: Vec<PathBuf>,
}

impl DummyImmutablesDbBuilder {
    /// [DummyImmutablesDbBuilder] factory, will create a folder with the given `dirname` in the
    /// system temp directory, if it exists already it will be cleaned.
    pub fn new(dir_name: &str) -> Self {
        Self {
            dir: Self::get_test_dir(dir_name),
            immutables_to_write: vec![],
            non_immutables_to_write: vec![],
            append_uncompleted_trio: false,
            file_size: None,
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

    /// Makes [build][Self::build] add another trio of immutables file, that won't be included
    /// in its returned vec, to simulate the last 3 'uncompleted / wip' files that can be found in
    /// a cardano immutable db.
    pub fn append_immutable_trio(&mut self) -> &mut Self {
        self.append_uncompleted_trio = true;
        self
    }

    /// Set the size of all files written by [build][Self::build] to the given `file_size` in bytes.
    ///
    /// Note: by default the size of the produced files is less than a 1kb.
    pub fn set_file_size(&mut self, file_size: u64) -> &mut Self {
        self.file_size = Some(file_size);
        self
    }

    /// Build a [DummyImmutableDb].
    pub fn build(&self) -> DummyImmutableDb {
        let mut non_immutables_files = vec![];
        let mut immutable_numbers = self.immutables_to_write.clone();
        immutable_numbers.sort();

        if self.append_uncompleted_trio {
            self.write_immutable_trio(match immutable_numbers.last() {
                None => 0,
                Some(last) => last + 1,
            });
        }

        for non_immutable in &self.non_immutables_to_write {
            non_immutables_files.push(self.write_dummy_file(non_immutable));
        }

        DummyImmutableDb {
            dir: self.dir.clone(),
            immutables_files: immutable_numbers
                .into_iter()
                .flat_map(|ifn| self.write_immutable_trio(ifn))
                .collect::<Vec<_>>(),
            non_immutables_files,
        }
    }

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        let parent_dir = std::env::temp_dir()
            .join("mithril_test")
            .join(subdir_name)
            .join("immutable");

        if parent_dir.exists() {
            std::fs::remove_dir_all(&parent_dir)
                .unwrap_or_else(|e| panic!("Could not remove dir {:?}: {}", parent_dir, e));
        }
        std::fs::create_dir_all(&parent_dir)
            .unwrap_or_else(|e| panic!("Could not create dir {:?}: {}", parent_dir, e));

        parent_dir
    }

    fn write_immutable_trio(&self, immutable: ImmutableFileNumber) -> Vec<ImmutableFile> {
        let mut result = vec![];
        for filename in [
            format!("{:05}.chunk", immutable),
            format!("{:05}.primary", immutable),
            format!("{:05}.secondary", immutable),
        ] {
            let file = self.write_dummy_file(&filename);
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
    fn write_dummy_file(&self, filename: &str) -> PathBuf {
        let file = self.dir.join(Path::new(filename));
        let mut source_file = File::create(&file).unwrap();

        write!(source_file, "This is a test file named '{}'", filename).unwrap();

        if let Some(file_size) = self.file_size {
            writeln!(source_file).unwrap();
            source_file.set_len(file_size).unwrap();
        }

        file
    }
}
