use std::path::Path;

use anyhow::anyhow;

use mithril_client::MithrilResult;

use super::{tar_gz_unpacker::TarGzUnpacker, zip_unpacker::ZipUnpacker, ArchiveFormat};

pub struct ArchiveUnpacker {
    supported_formats: Vec<Box<dyn ArchiveFormat>>,
}

impl Default for ArchiveUnpacker {
    fn default() -> Self {
        Self {
            supported_formats: vec![Box::new(TarGzUnpacker), Box::new(ZipUnpacker)],
        }
    }
}

impl ArchiveUnpacker {
    fn select_unpacker(&self, archive_path: &Path) -> MithrilResult<&dyn ArchiveFormat> {
        self.supported_formats
            .iter()
            .find(|f| f.supports(archive_path))
            .map(|f| f.as_ref())
            .ok_or_else(|| anyhow!("Unsupported archive format: {}", archive_path.display()))
    }

    pub fn unpack(&self, archive_path: &Path, unpack_dir: &Path) -> MithrilResult<()> {
        let unpacker = self.select_unpacker(archive_path)?;
        unpacker.unpack(archive_path, unpack_dir)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Write, path::Path};

    use flate2::{write::GzEncoder, Compression};
    use tar::{Builder, Header};
    use zip::{write::FileOptions, ZipWriter};

    use mithril_common::temp_dir_create;

    use super::*;

    #[test]
    fn archive_unpacker_unpacks_tar_gz_archive() {
        let temp_dir = temp_dir_create!();
        let archive_path = temp_dir.join("archive.tar.gz");

        {
            let tar_gz_file = File::create(&archive_path).unwrap();
            let encoder = GzEncoder::new(tar_gz_file, Compression::default());
            let mut tar_builder = Builder::new(encoder);

            let content = b"whatever content";
            let mut header = Header::new_gnu();
            header.set_size(content.len() as u64);
            header.set_cksum();
            tar_builder
                .append_data(&mut header, "file.txt", &content[..])
                .unwrap();
            tar_builder.finish().unwrap();
        }

        ArchiveUnpacker::default()
            .unpack(&archive_path, &temp_dir)
            .unwrap();

        assert!(temp_dir.join("file.txt").exists());
    }

    #[test]
    fn archive_unpacker_unpacks_zip_archive() {
        let temp_dir = temp_dir_create!();
        let archive_path = temp_dir.join("archive.zip");

        {
            let zip_file = File::create(&archive_path).unwrap();
            let mut zip_writer = ZipWriter::new(zip_file);

            zip_writer
                .start_file("file.txt", FileOptions::<()>::default())
                .unwrap();
            zip_writer.write_all(b"whatever content").unwrap();

            zip_writer.finish().unwrap();
        }

        ArchiveUnpacker::default()
            .unpack(&archive_path, &temp_dir)
            .unwrap();

        assert!(temp_dir.join("file.txt").exists());
    }

    #[test]
    fn fails_with_unknown_extension() {
        let path = Path::new("whatever.unknown");

        let archive_unpacker = ArchiveUnpacker::default();
        let result = archive_unpacker.select_unpacker(path);

        assert!(
            result.is_err(),
            "Should fail with unsupported archive extension."
        );
    }
}
