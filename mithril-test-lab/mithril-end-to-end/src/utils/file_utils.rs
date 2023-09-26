use anyhow::Context;
use mithril_common::StdResult;
use std::path::Path;
use tokio::process::Command;

/// Tail a file into into the given stream
///
/// For the sake of simplicity it use internally the tail command so be sure to have it on
/// your system.
pub async fn tail(file_path: &Path, number_of_line: u64) -> StdResult<String> {
    let tail_result = Command::new("tail")
        .args(vec![
            "-n",
            &number_of_line.to_string(),
            file_path.to_str().unwrap(),
        ])
        .kill_on_drop(true)
        .output()
        .await
        .with_context(|| format!("Failed to tail file `{}`", file_path.display()))?;

    String::from_utf8(tail_result.stdout).with_context(|| "Failed to parse tail output to utf8")
}

#[cfg(test)]
mod tests {
    use crate::utils::file_utils;
    use std::fs;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::{Path, PathBuf};

    fn get_temp_dir(subfolder_name: &str) -> PathBuf {
        let temp_dir = std::env::temp_dir()
            .join("mithril_test")
            .join(subfolder_name);
        if temp_dir.exists() {
            fs::remove_dir_all(&temp_dir)
                .unwrap_or_else(|_| panic!("Could not remove dir {temp_dir:?}"));
        }
        fs::create_dir_all(&temp_dir)
            .unwrap_or_else(|_| panic!("Could not create dir {temp_dir:?}"));

        temp_dir
    }

    fn write_file(path: &Path, file_content: &str) {
        let mut source_file = File::create(path).unwrap();
        write!(source_file, "{file_content}").unwrap();
    }

    #[tokio::test]
    pub async fn should_tail_file() {
        let file_path = get_temp_dir("should_tail_file").join("tail_file.txt");
        write_file(
            &file_path,
            r"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        tempor incididunt ut labore et dolore magna aliqua. Nibh venenatis cras sed felis eget
        velit aliquet. Sed adipiscing diam donec adipiscing tristique risus. Luctus accumsan
        tortor posuere ac.
        
        Bibendum enim facilisis gravida neque. Mi eget mauris pharetra et ultrices neque.
        Risus quis varius quam quisque id diam vel quam. Nunc sed velit dignissim sodales ut eu
        sem integer vitae. Integer eget aliquet nibh praesent.
        
        Senectus et netus et malesuada fames ac turpis. Neque convallis a cras semper auctor neque.
        Ac felis donec et odio pellentesque diam.
        Cras ornare arcu dui vivamus arcu felis.
        
        Enim ut tellus elementum sagittis vitae et.
        Aliquam malesuada bibendum arcu vitae elementum curabitur.
        Eu mi bibendum neque egestas congue quisque egestas diam.",
        );

        let tail_result = file_utils::tail(&file_path, 2)
            .await
            .expect("failed to tail file");

        assert_eq!(
            r"        Aliquam malesuada bibendum arcu vitae elementum curabitur.
        Eu mi bibendum neque egestas congue quisque egestas diam."
                .to_string(),
            tail_result
        );
    }
}
