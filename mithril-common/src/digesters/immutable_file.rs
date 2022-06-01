use crate::entities::ImmutableFileNumber;

use std::ffi::OsStr;
use std::io;
use std::num::ParseIntError;
use std::path::{Path, PathBuf};
use thiserror::Error;
use walkdir::WalkDir;

fn is_immutable(path: &Path) -> bool {
    let immutable = OsStr::new("immutable");
    path.iter().any(|component| component == immutable)
}

#[derive(Debug)]
pub struct ImmutableFile {
    pub path: PathBuf,
    pub number: ImmutableFileNumber,
}

#[derive(Error, Debug)]
pub enum ImmutableFileCreationError {
    #[error("Couldn't extract the file stem for '{path:?}'")]
    FileStemExtraction { path: PathBuf },
    #[error("Couldn't extract the filename as string for '{path:?}'")]
    FileNameExtraction { path: PathBuf },
    #[error("Error while parsing immutable file number")]
    FileNumberParsing(#[from] ParseIntError),
}

#[derive(Error, Debug)]
pub enum ImmutableFileListingError {
    #[error("metadata parsing failed")]
    MetadataParsing(#[from] io::Error),
    #[error("immutable file creation error")]
    ImmutableFileCreation(#[from] ImmutableFileCreationError),
}

impl ImmutableFile {
    pub fn new(path: PathBuf) -> Result<Self, ImmutableFileCreationError> {
        let filename = path
            .file_stem()
            .ok_or(ImmutableFileCreationError::FileStemExtraction { path: path.clone() })?;
        let filename = filename
            .to_str()
            .ok_or(ImmutableFileCreationError::FileNameExtraction { path: path.clone() })?;
        let immutable_file_number = filename.parse::<ImmutableFileNumber>()?;

        Ok(Self {
            path,
            number: immutable_file_number,
        })
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
        files.sort_by(|left, right| left.number.cmp(&right.number));

        Ok(files.into_iter().rev().skip(3).rev().collect())
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
                .expect(&*format!("Could not remove dir {:?}", parent_dir));
        }
        fs::create_dir_all(&parent_dir).expect(&*format!("Could not create dir {:?}", parent_dir));

        parent_dir
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(&file).unwrap();
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
}
