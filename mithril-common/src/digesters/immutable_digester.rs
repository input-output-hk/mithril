use crate::digesters::{Digester, DigesterError, DigesterResult};
use crate::entities::ImmutableFileNumber;

use sha2::{Digest, Sha256};
use slog::{debug, info, Logger};
use std::ffi::OsStr;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};

use walkdir::WalkDir;

/// A digester working directly on a Cardano DB immutables files
pub struct ImmutableDigester {
    /// A cardano node DB directory
    db_directory: PathBuf,

    /// The logger where the logs should be written
    logger: Logger,
}

impl ImmutableDigester {
    /// ImmutableDigester factory
    pub fn new(db_directory: PathBuf, logger: Logger) -> Self {
        Self {
            db_directory,
            logger,
        }
    }

    fn compute_hash(&self, entries: &[ImmutableFile]) -> Result<[u8; 32], io::Error> {
        let mut hasher = Sha256::new();
        let mut progress = Progress {
            index: 0,
            total: entries.len(),
        };

        for (ix, entry) in entries.iter().enumerate() {
            let mut file = File::open(&entry.path)?;

            io::copy(&mut file, &mut hasher)?;

            if progress.report(ix) {
                info!(self.logger, "hashing: {}", &progress);
            }
        }

        Ok(hasher.finalize().into())
    }
}

impl Digester for ImmutableDigester {
    fn compute_digest(&self) -> Result<DigesterResult, DigesterError> {
        let immutables = ImmutableFile::list_in_dir(&*self.db_directory)
            .map_err(DigesterError::ListImmutablesError)?;
        let last_immutable = immutables
            .last()
            .ok_or(DigesterError::NotEnoughImmutable())?;

        info!(self.logger, "#immutables: {}", immutables.len());

        let hash = self
            .compute_hash(&immutables)
            .map_err(DigesterError::DigestComputationError)?;
        let digest = hex::encode(hash);

        debug!(self.logger, "#computed digest: {:?}", digest);

        Ok(DigesterResult {
            digest,
            last_immutable_file_number: last_immutable.number,
        })
    }
}

struct Progress {
    index: usize,
    total: usize,
}

impl Progress {
    fn report(&mut self, ix: usize) -> bool {
        self.index = ix;
        (20 * ix) % self.total == 0
    }

    fn percent(&self) -> f64 {
        (self.index as f64 * 100.0 / self.total as f64).ceil()
    }
}

impl std::fmt::Display for Progress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}/{} ({}%)", self.index, self.total, self.percent())
    }
}

fn is_immutable(path: &Path) -> bool {
    let immutable = OsStr::new("immutable");
    path.iter().any(|component| component == immutable)
}

#[derive(Debug)]
struct ImmutableFile {
    path: PathBuf,
    number: ImmutableFileNumber,
}

impl ImmutableFile {
    fn new(path: PathBuf) -> Result<Self, String> {
        let filename = path
            .file_stem()
            .ok_or(format!("Couldn't extract the file stem for '{:?}'", path))?;
        let filename = filename.to_str().ok_or(format!(
            "Couldn't extract the filename as string for '{:?}'",
            path
        ))?;
        let immutable_file_number = filename
            .parse::<ImmutableFileNumber>()
            .map_err(|e| e.to_string())?;

        Ok(Self {
            path,
            number: immutable_file_number,
        })
    }

    /// List all [`ImmutableFile`] in a given directory.
    ///
    /// Important Note: It will skip the last chunk / primary / secondary trio since they're not yet
    /// complete.
    fn list_in_dir(dir: &Path) -> Result<Vec<ImmutableFile>, String> {
        let mut files: Vec<ImmutableFile> = vec![];

        for path in WalkDir::new(dir)
            .into_iter()
            .filter_map(|file| file.ok())
            .map(|f| f.path().to_owned())
        {
            let metadata = path.metadata().map_err(|e| e.to_string())?;
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
    use super::*;
    use std::fs;
    use std::io::prelude::*;

    const ROOT_FOLDER: &str = "tests-work-folder/";

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        let parent_dir = format!("{}{}", ROOT_FOLDER, subdir_name);
        let parent_dir = Path::new(&parent_dir).to_path_buf();

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

    // TODO: Test the case where number of immutable files is lower than 20

    #[test]
    fn reports_progress_every_5_percent() {
        let mut progress = Progress {
            index: 0,
            total: 7000,
        };

        assert!(!progress.report(1));
        assert!(!progress.report(4));
        assert!(progress.report(350));
        assert!(!progress.report(351));
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
        let result = ImmutableFile::list_in_dir(target_dir.parent().unwrap())
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
        let result = ImmutableFile::list_in_dir(target_dir.parent().unwrap())
            .expect("ImmutableFile::list_in_dir Failed");

        assert!(result.is_empty());
    }
}
