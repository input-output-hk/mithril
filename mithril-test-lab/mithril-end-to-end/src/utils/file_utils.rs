use anyhow::Context;
use mithril_common::StdResult;
use std::{path::Path, process::Stdio};
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

/// Grep last errors in a file and return them with context.
///
/// For the sake of simplicity it use internally the `tail` and `grep` commands so be sure to have them on
/// your system.
pub async fn last_errors(file_path: &Path, number_of_error: u64) -> StdResult<String> {
    let number_of_line_before = 5;
    let mut grep = Command::new("grep")
        .args(vec![
            "-n",
            "\"level\":50",
            file_path.to_str().unwrap(),
            "-B",
            &number_of_line_before.to_string(),
        ])
        .stdout(Stdio::piped())
        .spawn()
        .with_context(|| format!("Failed to spawn grep file `{}`", file_path.display()))?;

    let tail_stdin: Stdio = grep
        .stdout
        .take()
        .unwrap()
        .try_into()
        .with_context(|| format!("Failed to convert to Stdio."))?;

    let result = Command::new("tail")
        .args(vec![
            "-n",
            &(number_of_error * (number_of_line_before + 1)).to_string(),
        ])
        .stdin(tail_stdin)
        .kill_on_drop(true)
        .output()
        .await
        .with_context(|| format!("Failed to tail file `{}`", file_path.display()))?;

    String::from_utf8(result.stdout)
        .map(|s| s.replace("\\n", "\n"))
        .with_context(|| "Failed to parse command output to utf8")
}

#[cfg(test)]
mod tests {
    use crate::utils::file_utils;
    use mithril_common::test_utils::TempDir;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::{Path, PathBuf};

    fn get_temp_dir(subfolder_name: &str) -> PathBuf {
        TempDir::create("e2e-file-utils", subfolder_name)
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

    #[tokio::test]
    pub async fn should_return_last_error() {
        let temp_dir = get_temp_dir("should_return_last_error");
        let file_path = temp_dir.join("file_with_error.txt");
        write_file(
            &file_path,
            r#"{"msg":"A","content":"Ok"}
{"msg":"B","level":50,"content":"First error\n\nStack backtrace:\nline 1\nline 2"}
{"msg":"C","content":"Ok"}
{"msg":"D","content":"Ok"}
{"msg":"E","content":"Ok"}
{"msg":"F","content":"Ok"}
{"msg":"G","content":"Ok"}
{"msg":"H","content":"Ok"}
{"msg":"I","level":50,"content":"Second error\n\nStack backtrace:\nline 1\nline 2"}
{"msg":"J","level":70,"content":"other error"}
{"msg":"K","content":"Ok"}"#,
        );

        let error_result = file_utils::last_errors(&file_path, 1)
            .await
            .expect("failed to grep file");

        write_file(
            &temp_dir.join("expected.txt"),
            r#"4-{"msg":"D","content":"Ok"}
5-{"msg":"E","content":"Ok"}
6-{"msg":"F","content":"Ok"}
7-{"msg":"G","content":"Ok"}
8-{"msg":"H","content":"Ok"}
9-{"msg":"I","level":50,"content":"Second error

Stack backtrace:
line 1
line 2"}
"#,
        );

        write_file(&temp_dir.join("obtain.txt"), &error_result);

        assert_eq!(
            r#"4-{"msg":"D","content":"Ok"}
5-{"msg":"E","content":"Ok"}
6-{"msg":"F","content":"Ok"}
7-{"msg":"G","content":"Ok"}
8-{"msg":"H","content":"Ok"}
9:{"msg":"I","level":50,"content":"Second error

Stack backtrace:
line 1
line 2"}
"#
            .to_string(),
            error_result
        );
    }
}
