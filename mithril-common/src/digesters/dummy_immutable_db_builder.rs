#![cfg(test)]
use crate::{digesters::ImmutableFile, entities::ImmutableFileNumber};
use std::{
    fs::File,
    io::prelude::Write,
    path::{Path, PathBuf},
};

pub struct DummyImmutablesDbBuilder {
    dir: PathBuf,
    immutables_to_write: Vec<ImmutableFileNumber>,
    non_immutables_to_write: Vec<String>,
    append_uncompleted_trio: bool,
}

pub struct DummyImmutableDb {
    pub dir: PathBuf,
    pub immutables_files: Vec<ImmutableFile>,
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
        }
    }

    pub fn with_immutables(&mut self, immutables: &[ImmutableFileNumber]) -> &mut Self {
        self.immutables_to_write = immutables.to_vec();
        self
    }

    pub fn with_non_immutables(&mut self, non_immutables: &[&str]) -> &mut Self {
        self.non_immutables_to_write = non_immutables.iter().map(|f| f.to_string()).collect();
        self
    }

    /// Makes [Self::build] add another trio of immutables file, that won't be included
    /// in its returned vec, to simulate the last 3 'uncompleted / wip' files that can be found in
    /// a cardano immutable db.
    pub fn append_immutable_trio(&mut self) -> &mut Self {
        self.append_uncompleted_trio = true;
        self
    }

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
        file
    }
}
