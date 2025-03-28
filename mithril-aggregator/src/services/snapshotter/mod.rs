pub mod ancillary_signer;
mod compressed_archive_snapshotter;
mod interface;
mod test_doubles;

pub use compressed_archive_snapshotter::*;
pub use interface::*;
pub use test_doubles::*;

#[cfg(test)]
pub(crate) mod test_tools {
    use std::path::PathBuf;

    use mithril_common::test_utils::TempDir;

    pub fn get_test_directory(dir_name: &str) -> PathBuf {
        TempDir::create("snapshotter", dir_name)
    }
}
