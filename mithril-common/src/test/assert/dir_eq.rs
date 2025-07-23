use std::cmp::Ordering;
use std::fmt::Debug;
use std::path::{Path, PathBuf};

use walkdir::WalkDir;

/// A structure to compare two directories in a human-readable way in tests.
#[derive(Debug, Clone)]
pub struct DirStructure {
    content: String,
}

impl DirStructure {
    /// Creates a new `DirStructure` from a given path by recursively traversing it.
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let mut content = String::new();
        let mut is_first_entry = true;

        for dir_entry in WalkDir::new(path)
            .sort_by(|l, r| {
                (!l.file_type().is_dir())
                    .cmp(&!r.file_type().is_dir())
                    .then(l.file_name().cmp(r.file_name()))
            })
            .into_iter()
            .filter_entry(|e| e.file_type().is_file() || e.file_type().is_dir())
            .flatten()
            // Skip the first entry as it yields the root directory
            .skip(1)
        {
            if !is_first_entry {
                content.push('\n')
            } else {
                is_first_entry = false;
            }

            let suffix = if dir_entry.file_type().is_dir() {
                "/"
            } else {
                ""
            };

            content.push_str(&format!(
                "{} {}{suffix}",
                "*".repeat(dir_entry.depth()),
                dir_entry.file_name().to_string_lossy()
            ));
        }

        Self { content }
    }

    fn trimmed_lines(&self) -> Vec<&str> {
        self.content.lines().map(|l| l.trim_start()).collect()
    }

    /// Computes a line-by-line diff between the content of `self` and `other`,
    pub fn diff(&self, other: &Self) -> String {
        let mut self_lines = self.trimmed_lines();
        let mut other_lines = other.trimmed_lines();

        let left_padding = self_lines.iter().map(|l| l.len()).max().unwrap_or(0);

        // Equalize vector lengths by adding empty lines to the shorter one,
        // else zip will stop at the first missing line
        match self_lines.len().cmp(&other_lines.len()) {
            Ordering::Less => {
                let padding = vec![""; other_lines.len() - self_lines.len()];
                self_lines.extend(padding);
            }
            Ordering::Greater => {
                let padding = vec![""; self_lines.len() - other_lines.len()];
                other_lines.extend(padding);
            }
            Ordering::Equal => {}
        }

        self_lines
            .into_iter()
            .zip(other_lines)
            .map(|(left, right)| {
                if left == right {
                    format!("= {left}")
                } else {
                    format!("! {left:<left_padding$}  </>  {right}")
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl From<&Path> for DirStructure {
    fn from(path: &Path) -> Self {
        Self::from_path(path)
    }
}

impl PartialEq for DirStructure {
    fn eq(&self, other: &Self) -> bool {
        self.trimmed_lines() == other.trimmed_lines()
    }
}

impl From<PathBuf> for DirStructure {
    fn from(path: PathBuf) -> Self {
        Self::from_path(path)
    }
}

impl From<&PathBuf> for DirStructure {
    fn from(path: &PathBuf) -> Self {
        Self::from_path(path)
    }
}

impl From<String> for DirStructure {
    fn from(content: String) -> Self {
        Self { content }
    }
}

impl From<&str> for DirStructure {
    fn from(str: &str) -> Self {
        str.to_string().into()
    }
}

/// Compare a directory against a string representing its expected structure or against another
/// directory.
///
/// When comparing against a string, the string must be formatted as:
/// - one line per file or directory
/// - each line starts with a number of `*` representing the entry depth
/// - directories must end with a `/` (i.e.: `* folder/`)
/// - order rules are:
///   - directories then files
///   - alphanumeric order (i.e.: '20' comes before '3')
///
/// Example:
/// ```no_run
/// # use mithril_common::test::assert_dir_eq;
/// # use std::path::PathBuf;
/// # let path = PathBuf::new();
/// assert_dir_eq!(
///   &path,
///   "* folder_1/
///    ** file_1
///    ** file_2
///    ** subfolder/
///    *** subfolder_file
///    * file"
/// );
/// ```
#[macro_export]
macro_rules! assert_dir_eq {
    ($dir: expr, $expected_structure: expr) => {
        $crate::test::assert_dir_eq!($dir, $expected_structure, "");
    };
    ($dir: expr, $expected_structure: expr, $($arg:tt)+) => {
        let actual = $crate::test::DirStructure::from_path($dir);
        let expected = $crate::test::DirStructure::from($expected_structure);
        let comment = format!($($arg)+);
        assert!(
            actual == expected,
            "{}Directory `{}` does not match expected structure:
{}",
            if comment.is_empty() { String::new() } else { format!("{}:\n", comment) },
            $dir.display(),
            actual.diff(&expected)
        );
    };
}
pub use assert_dir_eq;

#[cfg(test)]
mod tests {
    use std::fs::{File, create_dir};

    use crate::test::temp_dir_create;

    use super::*;

    fn create_multiple_dirs<P: AsRef<Path>>(dirs: &[P]) {
        for dir in dirs {
            create_dir(dir).unwrap();
        }
    }

    fn create_multiple_files<P: AsRef<Path>>(files: &[P]) {
        for file in files {
            File::create(file).unwrap();
        }
    }

    #[test]
    fn path_to_dir_structure() {
        let test_dir = temp_dir_create!();

        assert_eq!("", DirStructure::from(&test_dir).content);

        create_dir(test_dir.join("folder1")).unwrap();
        assert_eq!("* folder1/", DirStructure::from(&test_dir).content);

        File::create(test_dir.join("folder1").join("file")).unwrap();
        assert_eq!(
            "* folder1/
** file",
            DirStructure::from(&test_dir).content
        );

        create_multiple_dirs(&[
            test_dir.join("folder2"),
            test_dir.join("folder2").join("f_subfolder"),
            test_dir.join("folder2").join("1_subfolder"),
        ]);
        create_multiple_files(&[
            test_dir.join("folder2").join("xyz"),
            test_dir.join("folder2").join("abc"),
            test_dir.join("folder2").join("100"),
            test_dir.join("folder2").join("20"),
            test_dir.join("folder2").join("300"),
            test_dir.join("main_folder_file"),
        ]);
        assert_eq!(
            "* folder1/
** file
* folder2/
** 1_subfolder/
** f_subfolder/
** 100
** 20
** 300
** abc
** xyz
* main_folder_file",
            DirStructure::from(&test_dir).content
        );
    }

    #[test]
    fn dir_structure_diff() {
        let structure = DirStructure {
            content: "* line 1\n* line 2".to_string(),
        };

        assert_eq!(
            "= * line 1
= * line 2",
            structure.diff(&structure)
        );
        assert_eq!(
            "!   </>  * line 1
!   </>  * line 2",
            DirStructure {
                content: String::new(),
            }
            .diff(&structure)
        );
        assert_eq!(
            "= * line 1
! * line 2  </>  ",
            structure.diff(&DirStructure {
                content: "* line 1".to_string(),
            })
        );
        assert_eq!(
            "! * line 1  </>  * line a
= * line 2
!           </>  * line b",
            structure.diff(&DirStructure {
                content: "* line a\n* line 2\n* line b".to_string(),
            })
        );
    }

    #[test]
    fn trim_whitespaces_at_lines_start() {
        let structure = DirStructure {
            content: "   * line1
            * line 2"
                .to_string(),
        };

        assert_eq!(vec!["* line1", "* line 2"], structure.trimmed_lines());
    }

    #[test]
    fn dir_eq_single_file() {
        let test_dir = temp_dir_create!();
        File::create(test_dir.join("file")).unwrap();
        assert_dir_eq!(&test_dir, "* file");
    }

    #[test]
    fn dir_eq_single_dir() {
        let test_dir = temp_dir_create!();
        create_dir(test_dir.join("folder")).unwrap();
        assert_dir_eq!(&test_dir, "* folder/");
    }

    #[test]
    fn can_compare_two_path() {
        let test_dir = temp_dir_create!();
        let left_dir = test_dir.join("left");
        let right_dir = test_dir.join("right");

        create_multiple_dirs(&[&left_dir, &right_dir]);
        create_multiple_files(&[left_dir.join("file"), right_dir.join("file")]);

        assert_dir_eq!(&left_dir, right_dir);
    }

    #[test]
    fn can_provide_additional_comment() {
        let test_dir = temp_dir_create!();
        assert_dir_eq!(&test_dir, "", "additional comment: {}", "formatted");
    }

    #[test]
    fn dir_eq_multiple_files_and_dirs() {
        let test_dir = temp_dir_create!();
        let first_subfolder = test_dir.join("folder 1");
        let second_subfolder = test_dir.join("folder 2");

        create_multiple_dirs(&[&first_subfolder, &second_subfolder]);
        create_multiple_files(&[
            test_dir.join("xyz"),
            test_dir.join("abc"),
            test_dir.join("100"),
            test_dir.join("20"),
            test_dir.join("300"),
            first_subfolder.join("file 1"),
            first_subfolder.join("file 2"),
            second_subfolder.join("file 3"),
        ]);

        assert_dir_eq!(
            &test_dir,
            "* folder 1/
             ** file 1
             ** file 2
             * folder 2/
             ** file 3
             * 100
             * 20
             * 300
             * abc
             * xyz"
        );
    }
}
