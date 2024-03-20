use std::ops::Not;
use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;
use human_bytes::human_bytes;
use thiserror::Error;

use mithril_client::{common::CompressionAlgorithm, MithrilError, MithrilResult};

/// Checks to apply before downloading a Cardano Db archive to a given directory.
pub struct CardanoDbDownloadChecker;

/// Errors tied with the [CardanoDbDownloadChecker].
#[derive(Debug, Error)]
pub enum CardanoDbDownloadCheckerError {
    /// Not enough space on the disk. There should be at least the ratio given for the
    /// used algorithm (see [CompressionAlgorithm::free_space_snapshot_ratio]) times
    /// the size of the archive to download to ensure it could be unpacked safely.
    #[error("There is only {} remaining in directory '{}' to store and unpack a {} large archive.", human_bytes(*left_space), pathdir.display(), human_bytes(*archive_size))]
    NotEnoughSpace {
        /// Left space on device
        left_space: f64,

        /// Specified location
        pathdir: PathBuf,

        /// Packed cardano db size
        archive_size: f64,
    },

    /// The directory where the files from cardano db are expanded is not empty.
    /// An error is raised to let the user handle what it wants to do with those
    /// files.
    #[error("Unpack directory '{0}' is not empty, please clean up it's content.")]
    UnpackDirectoryNotEmpty(PathBuf),

    /// Cannot write in the given directory.
    #[error("Unpack directory '{0}' is not writable, please check own or parents' permissions and ownership.")]
    UnpackDirectoryIsNotWritable(PathBuf, #[source] MithrilError),
}

impl CardanoDbDownloadChecker {
    /// Ensure that the given path exist, create it otherwise
    pub fn ensure_dir_exist(pathdir: &Path) -> MithrilResult<()> {
        if !pathdir.exists() {
            fs::create_dir_all(pathdir).map_err(|e| {
                CardanoDbDownloadCheckerError::UnpackDirectoryIsNotWritable(
                    pathdir.to_owned(),
                    e.into(),
                )
            })?;
        }

        Ok(())
    }

    /// Check all prerequisites are met before starting to download and unpack
    /// big cardano db archive.
    pub fn check_prerequisites(
        pathdir: &Path,
        size: u64,
        compression_algorithm: CompressionAlgorithm,
    ) -> MithrilResult<()> {
        Self::check_path_is_dir_and_writable(pathdir)?;
        Self::check_dir_writable(pathdir)?;
        Self::check_disk_space(pathdir, size, compression_algorithm)
    }

    fn check_path_is_dir_and_writable(pathdir: &Path) -> MithrilResult<()> {
        if pathdir.is_dir().not() {
            anyhow::bail!("Given path is not a directory: {}", pathdir.display());
        }

        if fs::read_dir(pathdir)
            .with_context(|| {
                format!(
                    "Could not list directory `{}` to check if it's empty",
                    pathdir.display()
                )
            })?
            .next()
            .is_some()
        {
            return Err(
                CardanoDbDownloadCheckerError::UnpackDirectoryNotEmpty(pathdir.to_owned()).into(),
            );
        }

        Ok(())
    }

    fn check_dir_writable(pathdir: &Path) -> MithrilResult<()> {
        // Check if the directory is writable by creating a temporary file
        let temp_file_path = pathdir.join("temp_file");
        fs::File::create(&temp_file_path).map_err(|e| {
            CardanoDbDownloadCheckerError::UnpackDirectoryIsNotWritable(
                pathdir.to_owned(),
                e.into(),
            )
        })?;

        // Delete the temporary file
        fs::remove_file(temp_file_path).map_err(|e| {
            CardanoDbDownloadCheckerError::UnpackDirectoryIsNotWritable(
                pathdir.to_owned(),
                e.into(),
            )
        })?;

        Ok(())
    }

    fn check_disk_space(
        pathdir: &Path,
        size: u64,
        compression_algorithm: CompressionAlgorithm,
    ) -> MithrilResult<()> {
        let free_space = fs2::available_space(pathdir)? as f64;
        if free_space < compression_algorithm.free_space_snapshot_ratio() * size as f64 {
            return Err(CardanoDbDownloadCheckerError::NotEnoughSpace {
                left_space: free_space,
                pathdir: pathdir.to_owned(),
                archive_size: size as f64,
            }
            .into());
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use mithril_common::test_utils::TempDir;

    use super::*;

    fn create_temporary_empty_directory(name: &str) -> PathBuf {
        TempDir::create("client-cli-unpacker", name)
    }

    #[test]
    fn create_directory_if_it_doesnt_exist() {
        let pathdir =
            create_temporary_empty_directory("directory_does_not_exist").join("target_directory");

        CardanoDbDownloadChecker::ensure_dir_exist(&pathdir)
            .expect("ensure_dir_exist should not fail");

        assert!(pathdir.exists());
    }

    #[test]
    fn return_error_if_path_is_a_file() {
        let pathdir =
            create_temporary_empty_directory("fail_if_pathdir_is_file").join("target_directory");
        fs::File::create(&pathdir).unwrap();

        CardanoDbDownloadChecker::ensure_dir_exist(&pathdir).unwrap();
        CardanoDbDownloadChecker::check_prerequisites(
            &pathdir,
            12,
            CompressionAlgorithm::default(),
        )
        .expect_err("check_prerequisites should fail");
    }

    #[test]
    fn return_ok_if_unpack_directory_does_not_exist() {
        let pathdir =
            create_temporary_empty_directory("directory_does_not_exist").join("target_directory");

        CardanoDbDownloadChecker::ensure_dir_exist(&pathdir).unwrap();
        CardanoDbDownloadChecker::check_prerequisites(
            &pathdir,
            12,
            CompressionAlgorithm::default(),
        )
        .expect("check_prerequisites should not fail");
    }

    #[test]
    fn return_ok_if_unpack_directory_exist_and_empty() {
        let pathdir =
            create_temporary_empty_directory("existing_directory").join("target_directory");
        fs::create_dir_all(&pathdir).unwrap();

        CardanoDbDownloadChecker::ensure_dir_exist(&pathdir).unwrap();
        CardanoDbDownloadChecker::check_prerequisites(
            &pathdir,
            12,
            CompressionAlgorithm::default(),
        )
        .expect("check_prerequisites should not fail");
    }

    #[test]
    fn return_error_if_unpack_directory_exists_and_not_empty() {
        let pathdir = create_temporary_empty_directory("existing_directory_not_empty");
        fs::create_dir_all(&pathdir).unwrap();
        fs::File::create(pathdir.join("file.txt")).unwrap();

        CardanoDbDownloadChecker::ensure_dir_exist(&pathdir).unwrap();
        let error = CardanoDbDownloadChecker::check_prerequisites(
            &pathdir,
            12,
            CompressionAlgorithm::default(),
        )
        .expect_err("check_prerequisites should fail");

        assert!(
            matches!(
                error.downcast_ref::<CardanoDbDownloadCheckerError>(),
                Some(CardanoDbDownloadCheckerError::UnpackDirectoryNotEmpty(_))
            ),
            "Unexpected error: {:?}",
            error
        );
    }

    #[test]
    fn return_error_if_not_enough_available_space() {
        let pathdir =
            create_temporary_empty_directory("enough_available_space").join("target_directory");
        let archive_size = u64::MAX;

        CardanoDbDownloadChecker::ensure_dir_exist(&pathdir).unwrap();
        let error = CardanoDbDownloadChecker::check_prerequisites(
            &pathdir,
            archive_size,
            CompressionAlgorithm::default(),
        )
        .expect_err("check_prerequisites should fail");

        assert!(
            matches!(
                error.downcast_ref::<CardanoDbDownloadCheckerError>(),
                Some(CardanoDbDownloadCheckerError::NotEnoughSpace {
                    left_space: _,
                    pathdir: _,
                    archive_size: _
                })
            ),
            "Unexpected error: {:?}",
            error
        );
    }

    // Those test are not on Windows because `set_readonly` is ignored for directories on Windows 7+
    // https://doc.rust-lang.org/std/fs/struct.Permissions.html#method.set_readonly
    #[cfg(not(target_os = "windows"))]
    mod unix_only {
        use super::*;

        fn make_readonly(path: &Path) {
            let mut perms = fs::metadata(path).unwrap().permissions();
            perms.set_readonly(true);
            fs::set_permissions(path, perms).unwrap();
        }

        #[test]
        fn return_error_if_directory_could_not_be_created() {
            let pathdir = create_temporary_empty_directory("read_only_directory");
            let targetdir = pathdir.join("target_directory");
            make_readonly(&pathdir);

            let error = CardanoDbDownloadChecker::ensure_dir_exist(&targetdir)
                .expect_err("ensure_dir_exist should fail");

            assert!(
                matches!(
                    error.downcast_ref::<CardanoDbDownloadCheckerError>(),
                    Some(CardanoDbDownloadCheckerError::UnpackDirectoryIsNotWritable(
                        _,
                        _
                    ))
                ),
                "Unexpected error: {:?}",
                error
            );
        }

        // This test is not run on Windows because `set_readonly` is ignored for directory on Windows 7+
        // https://doc.rust-lang.org/std/fs/struct.Permissions.html#method.set_readonly
        #[test]
        fn return_error_if_existing_directory_is_not_writable() {
            let pathdir =
                create_temporary_empty_directory("existing_directory_not_writable").join("db");
            fs::create_dir(&pathdir).unwrap();
            make_readonly(&pathdir);

            let error = CardanoDbDownloadChecker::check_prerequisites(
                &pathdir,
                12,
                CompressionAlgorithm::default(),
            )
            .expect_err("check_prerequisites should fail");

            assert!(
                matches!(
                    error.downcast_ref::<CardanoDbDownloadCheckerError>(),
                    Some(CardanoDbDownloadCheckerError::UnpackDirectoryIsNotWritable(
                        _,
                        _
                    ))
                ),
                "Unexpected error: {:?}",
                error
            );
        }
    }
}
