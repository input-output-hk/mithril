use std::{
    fs::{self, File},
    io,
    path::Path,
};

use anyhow::Context;
use zip::ZipArchive;

use mithril_client::MithrilResult;

use super::ArchiveFormat;

pub struct ZipUnpacker;

impl ArchiveFormat for ZipUnpacker {
    fn unpack(&self, archive_path: &Path, unpack_dir: &Path) -> MithrilResult<()> {
        let file = File::open(archive_path)
            .with_context(|| format!("Could not open archive file '{}'", archive_path.display()))?;
        let mut archive = ZipArchive::new(file)
            .with_context(|| format!("Could not read ZIP archive '{}'", archive_path.display()))?;

        for i in 0..archive.len() {
            let mut file = archive.by_index(i)?;
            let rel_path = file
                .enclosed_name()
                .ok_or_else(|| anyhow::anyhow!("File path is unsafe or malformed"))?;
            let outpath = unpack_dir.join(rel_path);

            if file.is_dir() {
                fs::create_dir_all(&outpath).with_context(|| {
                    format!("Could not create directory '{}'", outpath.display())
                })?;
            } else {
                if let Some(parent) = outpath.parent()
                    && !parent.exists()
                {
                    fs::create_dir_all(parent).with_context(|| {
                        format!("Could not create directory '{}'", parent.display())
                    })?;
                }
                let mut outfile = File::create(&outpath)
                    .with_context(|| format!("Could not create file '{}'", outpath.display()))?;
                io::copy(&mut file, &mut outfile)
                    .with_context(|| format!("Failed to write file '{}'", outpath.display()))?;
            }
        }

        Ok(())
    }

    fn supports(&self, path: &Path) -> bool {
        path.extension().and_then(|e| e.to_str()) == Some("zip")
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use zip::{ZipWriter, write::FileOptions};

    use mithril_common::{assert_dir_eq, temp_dir_create};

    use super::*;

    #[test]
    fn unpack_zip_archive_extracts_all_files() {
        let temp_dir = temp_dir_create!();
        let archive_path = temp_dir.join("archive.zip");

        {
            let zip_file = fs::File::create(&archive_path).unwrap();
            let mut zip_writer = ZipWriter::new(zip_file);

            zip_writer
                .start_file("root.txt", FileOptions::<()>::default())
                .unwrap();
            zip_writer.write_all(b"root content").unwrap();

            zip_writer
                .start_file("nested/dir/nested-file.txt", FileOptions::<()>::default())
                .unwrap();
            zip_writer.write_all(b"nested content").unwrap();

            zip_writer.finish().unwrap();
        }

        ZipUnpacker.unpack(&archive_path, &temp_dir).unwrap();

        assert_dir_eq! {
            &temp_dir,
            "* nested/
            ** dir/
            *** nested-file.txt
            * archive.zip
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
        assert!(ZipUnpacker.supports(Path::new("archive.zip")));
        assert!(ZipUnpacker.supports(Path::new("archive.whatever.zip")));
        assert!(!ZipUnpacker.supports(Path::new("archive.whatever")));
    }
}
