use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, anyhow};
use mithril_client::MithrilResult;

/// Copies a directory and its contents to a new location.
pub fn copy_dir(source_dir: &Path, target_dir: &Path) -> MithrilResult<PathBuf> {
    let source_dir_name = source_dir
        .file_name()
        .ok_or_else(|| anyhow!("Invalid source directory: {}", source_dir.display()))?;
    let destination_path = target_dir.join(source_dir_name);
    copy_dir_contents(source_dir, &destination_path)?;

    Ok(destination_path)
}

fn copy_dir_contents(source_dir: &Path, target_dir: &Path) -> MithrilResult<()> {
    fs::create_dir_all(target_dir).with_context(|| {
        format!(
            "Failed to create target directory: {}",
            target_dir.display()
        )
    })?;

    for entry in fs::read_dir(source_dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let src_path = entry.path();
        let dst_path = target_dir.join(entry.file_name());

        if file_type.is_dir() {
            copy_dir_contents(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path).with_context(|| {
                format!(
                    "Failed to copy file '{}' to '{}'",
                    src_path.display(),
                    dst_path.display()
                )
            })?;
        }
    }

    Ok(())
}

/// Removes all contents inside the given directory.
pub fn remove_dir_contents(dir: &Path) -> MithrilResult<()> {
    if !dir.exists() {
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            fs::remove_dir_all(&path)
                .with_context(|| format!("Failed to remove subdirectory: {}", path.display()))?;
        } else {
            fs::remove_file(&path)
                .with_context(|| format!("Failed to remove file: {}", path.display()))?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use mithril_common::{assert_dir_eq, temp_dir_create};

    use super::*;

    #[test]
    fn fails_if_source_does_not_exist() {
        let temp_dir = temp_dir_create!();
        let dir_not_exist = PathBuf::from("dir_not_exist");

        copy_dir(&dir_not_exist, &temp_dir)
            .expect_err("Expected error when source directory does not exist");
    }

    #[test]
    fn returns_copied_directory_path() {
        let temp_dir = temp_dir_create!();
        let src = temp_dir.join("dir_to_copy");
        fs::create_dir(&src).unwrap();
        let dst = temp_dir.join("dst");

        let copied_dir_path = copy_dir(&src, &dst).unwrap();

        assert_eq!(copied_dir_path, dst.join("dir_to_copy"));
    }

    #[test]
    fn copies_nested_directories_and_files() {
        let temp_dir = temp_dir_create!();
        let src = temp_dir.join("dir_to_copy");
        fs::create_dir(&src).unwrap();
        File::create(src.join("root.txt")).unwrap();

        let sub_dir1 = src.join("subdir1");
        fs::create_dir(&sub_dir1).unwrap();
        File::create(sub_dir1.join("subdir1.txt")).unwrap();

        let sub_dir2 = src.join("subdir2");
        fs::create_dir(&sub_dir2).unwrap();
        File::create(sub_dir2.join("subdir2.txt")).unwrap();

        let dst = temp_dir.join("dst");

        copy_dir(&src, &dst).unwrap();

        assert_dir_eq!(
            &dst,
            "* dir_to_copy/
            ** subdir1/
            *** subdir1.txt
            ** subdir2/
            *** subdir2.txt
            ** root.txt"
        );
    }

    #[test]
    fn cleans_directory_without_deleting_it() {
        let dir = temp_dir_create!().join("dir_to_clean");
        fs::create_dir(&dir).unwrap();

        File::create(dir.join("file1.txt")).unwrap();
        let sub_dir = dir.join("subdir");
        fs::create_dir(&sub_dir).unwrap();
        File::create(sub_dir.join("file2.txt")).unwrap();

        remove_dir_contents(&dir).unwrap();

        assert!(dir.exists());
        assert!(fs::read_dir(&dir).unwrap().next().is_none());
    }
}
