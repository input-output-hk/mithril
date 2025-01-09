use crate::entities::{ImmutableFileName, ImmutableFileNumber};

use crate::digesters::ImmutableFileListingError::MissingImmutableFolder;
use digest::{Digest, Output};
use std::{
    cmp::Ordering,
    fs::File,
    io,
    num::ParseIntError,
    path::{Path, PathBuf},
};
use thiserror::Error;
use walkdir::WalkDir;

const IMMUTABLE_FILE_EXTENSIONS: [&str; 3] = ["chunk", "primary", "secondary"];

fn is_immutable(entry: &walkdir::DirEntry) -> bool {
    let is_file = entry.file_type().is_file();
    let extension = entry.path().extension().map(|e| e.to_string_lossy());

    is_file && extension.is_some_and(|e| IMMUTABLE_FILE_EXTENSIONS.contains(&e.as_ref()))
}

/// Walk the given path and return the first directory named "immutable" it finds
fn find_immutables_dir(path_to_walk: &Path) -> Option<PathBuf> {
    WalkDir::new(path_to_walk)
        .into_iter()
        .filter_entry(|e| e.file_type().is_dir())
        .filter_map(|e| e.ok())
        .find(|f| f.file_name() == "immutable")
        .map(|e| e.into_path())
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
    #[error("Error while parsing immutable file number")]
    FileNumberParsing(#[from] ParseIntError),
}

/// [ImmutableFile::list_completed_in_dir] related errors.
#[derive(Error, Debug)]
pub enum ImmutableFileListingError {
    /// Raised when the metadata of a file could not be read.
    #[error("metadata parsing failed")]
    MetadataParsing(#[from] io::Error),

    /// Raised when [ImmutableFile::new] fails.
    #[error("immutable file creation error")]
    ImmutableFileCreation(#[from] ImmutableFileCreationError),

    /// Raised when the "immutable" folder could not be found in a file structure.
    #[error("Couldn't find the 'immutable' folder in '{0:?}'")]
    MissingImmutableFolder(PathBuf),
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

    cfg_test_tools! {
        /// ImmutableFile factory, TEST ONLY as it bypass the checks done by [ImmutableFile::new].
        pub fn dummy<T: Into<String>>(path: PathBuf, number: ImmutableFileNumber, filename: T) -> Self {
            Self {
                path,
                number,
                filename: filename.into(),
            }
        }
    }

    /// Compute the hash of this immutable file.
    pub fn compute_raw_hash<D>(&self) -> Result<Output<D>, io::Error>
    where
        D: Digest + io::Write,
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
        let immutable_dir =
            find_immutables_dir(dir).ok_or(MissingImmutableFolder(dir.to_path_buf()))?;
        let mut files: Vec<ImmutableFile> = vec![];

        for path in WalkDir::new(immutable_dir)
            .min_depth(1)
            .max_depth(1)
            .into_iter()
            .filter_entry(is_immutable)
            .filter_map(|file| file.ok())
        {
            let immutable_file = ImmutableFile::new(path.into_path())?;
            files.push(immutable_file);
        }
        files.sort();

        match files.last() {
            // empty list
            None => Ok(files),
            // filter out the last immutable file(s)
            Some(last_file) => {
                let last_number = last_file.number;
                Ok(files
                    .into_iter()
                    .filter(|f| f.number < last_number)
                    .collect())
            }
        }
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
    use crate::test_utils::TempDir;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::{Path, PathBuf};

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        TempDir::create("immutable_file", subdir_name)
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(file).unwrap();
            write!(source_file, "This is a test file named '{filename}'").unwrap();
        }
    }

    fn extract_filenames(immutables: &[ImmutableFile]) -> Vec<String> {
        immutables
            .iter()
            .map(|i| i.path.file_name().unwrap().to_str().unwrap().to_owned())
            .collect()
    }

    #[test]
    fn list_immutable_file_fail_if_not_in_immutable_dir() {
        let target_dir = get_test_dir("list_immutable_file_fail_if_not_in_immutable_dir/invalid");
        let entries = vec![];
        create_fake_files(&target_dir, &entries);

        ImmutableFile::list_completed_in_dir(target_dir.parent().unwrap())
            .expect_err("ImmutableFile::list_in_dir should have Failed");
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
        let immutables_names: Vec<String> = extract_filenames(&immutables);

        let expected: Vec<&str> = entries.into_iter().rev().skip(3).rev().collect();
        assert_eq!(expected, immutables_names);
    }

    #[test]
    fn list_immutable_file_should_work_with_non_immutable_files() {
        let target_dir =
            get_test_dir("list_immutable_file_should_work_with_non_immutable_files/immutable");
        let entries = vec![
            "123.chunk",
            "123.primary",
            "123.secondary",
            "124.chunk",
            "124.primary",
            "124.secondary",
            "README.md",
            "124.secondary.back",
        ];
        create_fake_files(&target_dir, &entries);
        let immutables = ImmutableFile::list_completed_in_dir(target_dir.parent().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");
        let immutables_names: Vec<String> = extract_filenames(&immutables);

        let expected: Vec<&str> = entries.into_iter().rev().skip(5).rev().collect();
        assert_eq!(expected, immutables_names);
    }

    #[test]
    fn list_immutable_file_can_list_incomplete_trio() {
        let target_dir = get_test_dir("list_immutable_file_can_list_incomplete_trio/immutable");
        let entries = vec![
            "21.chunk",
            "21.primary",
            "21.secondary",
            "123.chunk",
            "123.secondary",
            "124.chunk",
            "124.primary",
            "125.primary",
            "125.secondary",
            "223.chunk",
            "224.primary",
            "225.secondary",
            "226.chunk",
        ];
        create_fake_files(&target_dir, &entries);
        let immutables = ImmutableFile::list_completed_in_dir(target_dir.parent().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");
        let immutables_names: Vec<String> = extract_filenames(&immutables);

        let expected: Vec<&str> = entries.into_iter().rev().skip(1).rev().collect();
        assert_eq!(expected, immutables_names);
    }
}
