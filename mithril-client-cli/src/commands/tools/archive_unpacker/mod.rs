mod tar_gz_unpacker;
mod zip_unpacker;

pub use tar_gz_unpacker::*;
pub use zip_unpacker::*;

use mithril_client::MithrilResult;
use std::path::Path;

#[cfg_attr(test, mockall::automock)]
pub trait ArchiveUnpacker {
    fn unpack(&self, archive_path: &Path, unpack_dir: &Path) -> MithrilResult<()>;

    #[cfg(test)]
    fn as_any(&self) -> &dyn std::any::Any;
}
