mod api;
pub mod appender;
mod entities;

pub use api::*;
pub use entities::*;

// Todo: unduplicate this code
#[cfg(test)]
pub(crate) mod test_tools {
    use flate2::read::GzDecoder;
    use std::fs::File;
    use std::path::{Path, PathBuf};
    use tar::Archive;

    use mithril_common::test_utils::TempDir;

    use super::*;

    pub fn get_test_directory(dir_name: &str) -> PathBuf {
        TempDir::create("file_archiver", dir_name)
    }

    /// Create a file in the root directory.
    ///
    /// Returns the relative path to the created file based on the root directory.
    pub fn create_file(root: &Path, filename: &str) -> PathBuf {
        let file_path = PathBuf::from(filename);
        File::create(root.join(file_path.clone())).unwrap();
        file_path
    }

    /// Create a directory in the root directory.
    ///
    /// Returns the relative path to the created directory based on the root directory.
    pub fn create_dir(root: &Path, dirname: &str) -> PathBuf {
        let dir_path = PathBuf::from(dirname);
        std::fs::create_dir(root.join(dir_path.clone())).unwrap();
        dir_path
    }

    pub fn unpack_gz_decoder<P: AsRef<Path>>(test_dir: P, file_archive: FileArchive) -> PathBuf {
        let test_dir = test_dir.as_ref();
        let file_tar_gz = File::open(file_archive.get_file_path()).unwrap();
        let file_tar_gz_decoder = GzDecoder::new(file_tar_gz);
        let mut archive = Archive::new(file_tar_gz_decoder);
        let unpack_path = test_dir.join(create_dir(test_dir, "unpack"));
        archive.unpack(&unpack_path).unwrap();

        unpack_path
    }
}
