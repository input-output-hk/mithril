use std::path::{Path, PathBuf};

/// A builder of temp directory for tests purpose.
#[derive(Clone)]
pub struct TempDir {
    module_name: String,
    name: String,
    enable_short_path: bool,
}

const TEMP_DIR_ROOT_NAME: &str = "mithril_test";

impl TempDir {
    /// `TempDir` builder factory
    pub fn new<T: Into<String>>(module: T, name: T) -> Self {
        Self {
            module_name: module.into(),
            name: name.into(),
            enable_short_path: false,
        }
    }

    /// Change path generation in order to guarantee a path that have at maximum 90 characters.
    ///
    /// Typically used for cases when the generated folder will include a socket.
    pub fn generate_shorter_path(mut self) -> Self {
        self.enable_short_path = true;
        self
    }

    /// Generate the path of the temp directory (no IO operation will be executed)
    pub fn build_path(&self) -> PathBuf {
        let base_dir = std::env::temp_dir().join(TEMP_DIR_ROOT_NAME);

        if self.enable_short_path {
            // Combined max len should be lower than 90 to have some rooms for the folder content
            // macOS temp folder are not in the `/tmp` folder but in a dynamic path adding 45 chars
            // ie: /var/folders/_k/7j0m5c_n4g94vgx9gxknp4tm0000gn/T/
            let max_len = self.name.len().min(89 - base_dir.to_string_lossy().len());

            base_dir.join(&self.name[0..max_len])
        } else {
            base_dir.join(&self.module_name).join(&self.name)
        }
    }

    /// Create a directory based of builder configuration in the system temp folder.
    pub fn build(&self) -> PathBuf {
        let path = self.build_path();
        self.create_dir(&path);

        path
    }

    /// Create on disk a temp directory based on the given module & name.
    ///
    /// Equivalent to:
    /// ```
    /// # use crate::mithril_common::test_utils::TempDir;
    /// TempDir::new("module", "name").build();
    /// ```
    pub fn create<T: Into<String>>(module: T, name: T) -> PathBuf {
        Self::new(module, name).build()
    }

    /// Create on disk a temp directory based on the given module & name, the generated path
    /// is guaranteed to be at most 90 characters long.
    ///
    /// Equivalent to:
    /// ```
    /// # use crate::mithril_common::test_utils::TempDir;
    /// TempDir::new("module", "name").generate_shorter_path().build();
    /// ```
    pub fn create_with_short_path<T: Into<String>>(module: T, name: T) -> PathBuf {
        Self::new(module, name).generate_shorter_path().build()
    }

    fn create_dir(&self, path: &Path) {
        if path.exists() {
            std::fs::remove_dir_all(path)
                .unwrap_or_else(|e| panic!("Could not remove dir {path:?}: {e}"));
        }

        std::fs::create_dir_all(path)
            .unwrap_or_else(|e| panic!("Could not create dir {path:?}: {e}"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn non_short_path_are_in_a_mithril_test_slash_module_folder_structure() {
        let path = TempDir::new("mod", "name").build();

        assert_eq!(
            Some(
                std::env::temp_dir()
                    .join(TEMP_DIR_ROOT_NAME)
                    .join("mod")
                    .as_path()
            ),
            path.parent()
        );
        assert!(path.exists());
    }

    #[test]
    fn short_path_are_in_a_mithril_test_folder_structure() {
        let path = TempDir::new("mod", "name").generate_shorter_path().build();

        assert_eq!(
            Some(std::env::temp_dir().join(TEMP_DIR_ROOT_NAME).as_path()),
            path.parent()
        );
        assert!(path.exists());
    }

    #[test]
    fn shorter_path_have_a_length_lower_than_90_chars_even_when_given_module_longer_than_that() {
        let path = TempDir::new(
            "module_longer_than_a_string_of_90_characters_so_this_test_can_fail_if_the_builder_is_a_bad_builder_that_do_nothing",
            "name",
        )
            .generate_shorter_path()
            .build_path();
        let path_len = path.to_string_lossy().len();

        assert!(
            path_len <= 90,
            "path with `short` option enabled was longer than 90 characters:\n\
            path_len: `{path_len}`\n\
            path: `{}`",
            path.display()
        );
    }

    #[test]
    fn shorter_path_have_a_length_lower_than_90_chars_even_when_given_name_longer_than_that() {
        let path = TempDir::new(
            "mod",
            "name_longer_than_a_string_of_90_characters_so_this_test_can_fail_if_the_builder_is_a_bad_builder_that_do_nothing",
        )
            .generate_shorter_path()
            .build_path();
        let path_len = path.to_string_lossy().len();

        assert!(
            path_len <= 90,
            "path with `short` option enabled was longer than 90 characters:\n\
            path_len: `{path_len}`\n\
            path: `{}`",
            path.display()
        );
    }
}
