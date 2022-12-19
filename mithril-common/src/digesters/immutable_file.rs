use crate::entities::{ImmutableFileName, ImmutableFileNumber};

use digest::{Digest, Output};
use std::{
    cmp::Ordering,
    ffi::OsStr,
    fs::File,
    io,
    num::ParseIntError,
    path::{Path, PathBuf},
};
use thiserror::Error;
use walkdir::WalkDir;

fn is_immutable(path: &Path) -> bool {
    let immutable = OsStr::new("immutable");
    path.iter().any(|component| component == immutable)
}

/// Represent an immutable file in a Cardano node database directory
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ImmutableFile {
    /// The path to the immutable file
    pub path: PathBuf,

    /// The immutable file number
    pub number: ImmutableFileNumber,

    /// The filename
    pub filename: ImmutableFileName,
}

/// [ImmutableFile::new] related errors.
#[derive(Error, Debug)]
pub enum ImmutableFileCreationError {
    /// Raised when the immutable file stem extraction fails.
    #[error("Couldn't extract the file stem for '{path:?}'")]
    FileStemExtraction {
        /// Path for which file stem extraction failed.
        path: PathBuf,
    },

    /// Raised when the immutable file filename extraction fails.
    #[error("Couldn't extract the filename as string for '{path:?}'")]
    FileNameExtraction {
        /// Path for which filename extraction failed.
        path: PathBuf,
    },

    /// Raised when the immutable file number parsing, from the filename, fails.
    #[error("Error while parsing immutable file number: {0}")]
    FileNumberParsing(#[from] ParseIntError),
}

/// [ImmutableFile::list_completed_in_dir] related errors.
#[derive(Error, Debug)]
pub enum ImmutableFileListingError {
    /// Raised when the metadata of a file could not be read.
    #[error("metadata parsing failed: {0}")]
    MetadataParsing(#[from] io::Error),

    /// Raised when [ImmutableFile::new] fails.
    #[error("immutable file creation error: {0}")]
    ImmutableFileCreation(#[from] ImmutableFileCreationError),
}

impl ImmutableFile {
    /// ImmutableFile factory
    pub fn new(path: PathBuf) -> Result<ImmutableFile, ImmutableFileCreationError> {
        let filename = path
            .file_name()
            .ok_or(ImmutableFileCreationError::FileNameExtraction { path: path.clone() })?
            .to_str()
            .ok_or(ImmutableFileCreationError::FileNameExtraction { path: path.clone() })?
            .to_string();

        let filestem = path
            .file_stem()
            .ok_or(ImmutableFileCreationError::FileStemExtraction { path: path.clone() })?
            .to_str()
            .ok_or(ImmutableFileCreationError::FileNameExtraction { path: path.clone() })?;
        let immutable_file_number = filestem.parse::<ImmutableFileNumber>()?;

        Ok(Self {
            path,
            number: immutable_file_number,
            filename,
        })
    }

    /// ImmutableFile factory, TEST ONLY as it bypass the checks done by [ImmutableFile::new].
    #[cfg(test)]
    pub(crate) fn dummy(path: PathBuf, number: ImmutableFileNumber, filename: String) -> Self {
        Self {
            path,
            number,
            filename,
        }
    }

    /// Compute the hash of this immutable file.
    pub fn compute_raw_hash<D: Digest>(&self) -> Result<Output<D>, io::Error>
    where
        D: io::Write,
    {
        let mut hasher = D::new();
        let mut file = File::open(&self.path)?;
        io::copy(&mut file, &mut hasher)?;
        Ok(hasher.finalize())
    }

    /// List all [`ImmutableFile`] in a given directory.
    ///
    /// Important Note: It will skip the last chunk / primary / secondary trio since they're not yet
    /// complete.
    pub fn list_completed_in_dir(
        dir: &Path,
    ) -> Result<Vec<ImmutableFile>, ImmutableFileListingError> {
        let mut files: Vec<ImmutableFile> = vec![];

        for path in WalkDir::new(dir)
            .into_iter()
            .filter_map(|file| file.ok())
            .map(|f| f.path().to_owned())
        {
            let metadata = path.metadata()?;
            if metadata.is_file() && is_immutable(&path) {
                let immutable_file = ImmutableFile::new(path)?;
                files.push(immutable_file);
            }
        }
        files.sort();

        // @todo: make the skip of the last 'trio' more robust
        Ok(files.into_iter().rev().skip(3).rev().collect())
    }
}

impl PartialOrd for ImmutableFile {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ImmutableFile {
    fn cmp(&self, other: &Self) -> Ordering {
        self.number
            .cmp(&other.number)
            .then(self.path.cmp(&other.path))
    }
}

#[cfg(test)]
mod tests {
    use super::ImmutableFile;
    use std::fs;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::{Path, PathBuf};

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        let parent_dir = std::env::temp_dir().join("mithril_test").join(subdir_name);

        if parent_dir.exists() {
            fs::remove_dir_all(&parent_dir)
                .unwrap_or_else(|_| panic!("Could not remove dir {:?}", parent_dir));
        }
        fs::create_dir_all(&parent_dir)
            .unwrap_or_else(|_| panic!("Could not create dir {:?}", parent_dir));

        parent_dir
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(file).unwrap();
            write!(source_file, "This is a test file named '{}'", filename).unwrap();
        }
    }

    #[test]
    fn list_immutable_file_should_skip_last_number() {
        let target_dir = get_test_dir("list_immutable_file_should_skip_last_number/immutable");
        let entries = vec![
            "123.chunk",
            "123.primary",
            "123.secondary",
            "125.chunk",
            "125.primary",
            "125.secondary",
            "0124.chunk",
            "0124.primary",
            "0124.secondary",
            "223.chunk",
            "223.primary",
            "223.secondary",
            "0423.chunk",
            "0423.primary",
            "0423.secondary",
            "0424.chunk",
            "0424.primary",
            "0424.secondary",
            "21.chunk",
            "21.primary",
            "21.secondary",
        ];
        create_fake_files(&target_dir, &entries);
        let result = ImmutableFile::list_completed_in_dir(target_dir.parent().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");

        assert_eq!(result.last().unwrap().number, 423);
        assert_eq!(
            result.len(),
            entries.len() - 3,
            "Expected to find {} files since The last (chunk, primary, secondary) trio is skipped, but found {}",
            entries.len() - 3,
            result.len(),
        );
    }

    #[test]
    fn list_immutable_file_should_works_in_a_empty_folder() {
        let target_dir =
            get_test_dir("list_immutable_file_should_works_even_in_a_empty_folder/immutable");
        let entries = vec![];
        create_fake_files(&target_dir, &entries);
        let result = ImmutableFile::list_completed_in_dir(target_dir.parent().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");

        assert!(result.is_empty());
    }

    #[test]
    fn immutable_order_should_be_deterministic() {
        let target_dir = get_test_dir("immutable_order_should_be_deterministic/immutable");
        let entries = vec![
            "21.chunk",
            "21.primary",
            "21.secondary",
            "123.chunk",
            "123.primary",
            "123.secondary",
            "124.chunk",
            "124.primary",
            "124.secondary",
            "125.chunk",
            "125.primary",
            "125.secondary",
            "223.chunk",
            "223.primary",
            "223.secondary",
            "423.chunk",
            "423.primary",
            "423.secondary",
            "424.chunk",
            "424.primary",
            "424.secondary",
        ];
        create_fake_files(&target_dir, &entries);
        let immutables = ImmutableFile::list_completed_in_dir(target_dir.parent().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");
        let immutables_names: Vec<String> = immutables
            .into_iter()
            .map(|i| i.path.file_name().unwrap().to_str().unwrap().to_owned())
            .collect();

        let expected: Vec<&str> = entries.into_iter().rev().skip(3).rev().collect();
        assert_eq!(expected, immutables_names);
    }
}
