mod tar_gz_unpacker;
mod unpacker;
mod zip_unpacker;

pub use unpacker::*;

use mithril_client::MithrilResult;
use std::path::Path;

/// Trait for supported archive formats (e.g. `.zip`, `.tar.gz`).
#[cfg_attr(test, mockall::automock)]
pub trait ArchiveFormat {
    /// Checks whether this format can handle the given archive file.
    fn supports(&self, archive_path: &Path) -> bool;

    /// Unpacks the archive into the target directory.
    fn unpack(&self, archive_path: &Path, unpack_dir: &Path) -> MithrilResult<()>;
}
