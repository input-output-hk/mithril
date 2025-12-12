use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;

use crate::MithrilResult;

/// Create a directory if it does not exist
pub fn create_directory_if_not_exists(dir: &Path) -> MithrilResult<()> {
    if dir.exists() {
        return Ok(());
    }

    fs::create_dir_all(dir).with_context(|| "Failed creating directory")
}

/// Delete a directory if it exists
pub fn delete_directory(dir: &Path) -> MithrilResult<()> {
    if dir.exists() {
        fs::remove_dir_all(dir).with_context(|| "Failed deleting directory")?;
    }

    Ok(())
}

/// Read files in a directory
pub fn read_files_in_directory(dir: &Path) -> MithrilResult<Vec<PathBuf>> {
    let mut files = vec![];
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            files.push(path);
        }
    }

    Ok(files)
}
