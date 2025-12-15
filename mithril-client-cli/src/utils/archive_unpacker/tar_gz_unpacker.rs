use std::{fs::File, path::Path};

use anyhow::Context;
use flate2::read::GzDecoder;
use mithril_client::MithrilResult;
use tar::Archive;

use super::ArchiveFormat;

#[derive(Debug, Eq, PartialEq)]
pub struct TarGzUnpacker;

impl ArchiveFormat for TarGzUnpacker {
    fn unpack(&self, archive_path: &Path, unpack_dir: &Path) -> MithrilResult<()> {
        let archive = File::open(archive_path)
            .with_context(|| format!("Could not open archive file '{}'", archive_path.display()))?;
        let gzip_decoder = GzDecoder::new(archive);
        let mut file_archive = Archive::new(gzip_decoder);
        file_archive.unpack(unpack_dir).with_context(|| {
            format!(
                "Could not unpack '{}' with 'Gzip' to directory '{}'",
                archive_path.display(),
                unpack_dir.display()
            )
        })?;

        Ok(())
    }

    fn supports(&self, path: &Path) -> bool {
        path.extension().and_then(|e| e.to_str()) == Some("gz")
    }
}

#[cfg(test)]
mod tests {
    use std::fs::{self, File};

    use flate2::{Compression, write::GzEncoder};
    use mithril_common::{assert_dir_eq, temp_dir_create};
    use tar::{Builder, Header};

    use super::*;

    #[test]
    fn unpack_tar_archive_extracts_all_files() {
        let temp_dir = temp_dir_create!();
        let archive_path = temp_dir.join("archive.tar.gz");

        {
            let tar_gz_file = File::create(&archive_path).unwrap();
            let encoder = GzEncoder::new(tar_gz_file, Compression::default());
            let mut tar_builder = Builder::new(encoder);

            let content = b"root content";
            let mut header = Header::new_gnu();
            header.set_size(content.len() as u64);
            header.set_cksum();
            tar_builder
                .append_data(&mut header, "root.txt", &content[..])
                .unwrap();

            let content = b"nested content";
            let mut header = Header::new_gnu();
            header.set_size(content.len() as u64);
            header.set_cksum();
            tar_builder
                .append_data(&mut header, "nested/dir/nested-file.txt", &content[..])
                .unwrap();

            tar_builder.finish().unwrap();
        }

        TarGzUnpacker.unpack(&archive_path, &temp_dir).unwrap();

        assert_dir_eq! {
            &temp_dir,
            "* nested/
            ** dir/
            *** nested-file.txt
            * archive.tar.gz
            * root.txt"
        };

        let root_file_content = fs::read_to_string(temp_dir.join("root.txt")).unwrap();
        assert_eq!(root_file_content, "root content");

        let nested_file_content =
            fs::read_to_string(temp_dir.join("nested/dir/nested-file.txt")).unwrap();
        assert_eq!(nested_file_content, "nested content");
    }

    #[test]
    fn supported_file_extension() {
        assert!(TarGzUnpacker.supports(Path::new("archive.tar.gz")));
        assert!(TarGzUnpacker.supports(Path::new("archive.gz")));
        assert!(!TarGzUnpacker.supports(Path::new("archive.tar")));
        assert!(!TarGzUnpacker.supports(Path::new("archive.whatever")));
    }
}
