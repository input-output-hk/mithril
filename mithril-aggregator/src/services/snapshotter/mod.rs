mod appender;
mod compressed_archive_snapshotter;
mod dumb_snapshotter;
mod interface;

pub use compressed_archive_snapshotter::*;
pub use dumb_snapshotter::*;
pub use interface::*;

#[cfg(test)]
pub(crate) mod test_tools {
    use flate2::read::GzDecoder;
    use std::fs::File;
    use std::path::{Path, PathBuf};
    use tar::Archive;
    use uuid::Uuid;

    use mithril_common::test_utils::TempDir;

    use crate::services::OngoingSnapshot;

    pub fn get_test_directory(dir_name: &str) -> PathBuf {
        TempDir::create("snapshotter", dir_name)
    }

    pub fn create_file(root: &Path, filename: &str) -> PathBuf {
        let file_path = PathBuf::from(filename);
        File::create(root.join(file_path.clone())).unwrap();
        file_path
    }

    pub fn create_dir(root: &Path, dirname: &str) -> PathBuf {
        let dir_path = PathBuf::from(dirname);
        std::fs::create_dir(root.join(dir_path.clone())).unwrap();
        dir_path
    }

    // Generate unique name for the archive is mandatory to avoid conflicts during the verification.
    pub fn random_archive_name() -> String {
        format!("{}.tar.gz", Uuid::new_v4())
    }

    pub fn unpack_gz_decoder(test_dir: PathBuf, snapshot: OngoingSnapshot) -> PathBuf {
        let file_tar_gz = File::open(snapshot.get_file_path()).unwrap();
        let file_tar_gz_decoder = GzDecoder::new(file_tar_gz);
        let mut archive = Archive::new(file_tar_gz_decoder);
        let unpack_path = test_dir.join(create_dir(&test_dir, "unpack"));
        archive.unpack(&unpack_path).unwrap();

        unpack_path
    }
}
