use std::path::Path;

use anyhow::anyhow;

use mithril_client::MithrilResult;

use super::archive_unpacker::{ArchiveUnpacker, TarGzUnpacker, ZipUnpacker};

pub struct ArchiveUnpackerSelector;

impl ArchiveUnpackerSelector {
    pub fn select_unpacker(path: &Path) -> MithrilResult<Box<dyn ArchiveUnpacker>> {
        let extension = path
            .extension()
            .and_then(|e| e.to_str())
            .ok_or_else(|| anyhow!("Invalid or missing file extension: {}", path.display()))?;

        match extension {
            "gz" => Ok(Box::new(TarGzUnpacker)),
            "zip" => Ok(Box::new(ZipUnpacker)),
            _ => Err(anyhow!(
                "Unsupported archive format: {}. Supported formats are 'gz' and 'zip'",
                extension
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::commands::tools::archive_unpacker::{TarGzUnpacker, ZipUnpacker};

    use super::*;

    #[test]
    fn select_tar_gz_unpacker() {
        let path = Path::new("whatever.tar.gz");

        let unpacker = ArchiveUnpackerSelector::select_unpacker(path).unwrap();

        assert!(unpacker.as_ref().as_any().is::<TarGzUnpacker>());
    }

    #[test]
    fn select_zip_unpacker() {
        let path = Path::new("whatever.zip");

        let unpacker = ArchiveUnpackerSelector::select_unpacker(path).unwrap();

        assert!(unpacker.as_ref().as_any().is::<ZipUnpacker>());
    }

    #[test]
    fn fails_with_unknown_extension() {
        let path = Path::new("whatever.unknown");

        let result = ArchiveUnpackerSelector::select_unpacker(path);

        assert!(result.is_err());
    }
}
