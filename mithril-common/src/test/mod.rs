//! Test utilities
//!
//! They contains:
//! * A Open Api Spec tester
//! * Some precomputed fake data and keys
//! * A builder of [MithrilFixture] to generate signers alongside a stake distribution
//!

pub mod builder;
pub mod double;
pub mod logging;
pub mod mock_extensions;

mod assert;
mod temp_dir;

pub use assert::*;
pub use temp_dir::*;

logging::define_test_logger!();

/// Return the path of the given function.
/// If the last function is `f`, it is removed.
/// The last `{{closure}}` is also removed.
pub fn format_current_function_module<T>(f: T) -> &'static str {
    fn type_name_of<T>(_: T) -> &'static str {
        std::any::type_name::<T>()
    }

    let name = type_name_of(f);
    let name = name.strip_suffix("::f").unwrap_or(name);
    name.strip_suffix("::{{closure}}").unwrap_or(name)
}

/// Return a string representing the path of the given function.
pub fn format_current_function_path<T>(f: T) -> String {
    let name = format_current_function_module(f);
    name.replace("::", "/")
}

/// Returns the name of the function that called this macro.
#[macro_export]
macro_rules! current_function {
    () => {{
        fn f() {}
        let name = $crate::test::format_current_function_module(f);
        // The index found is the beginning of the '..', this is why we add 2.
        let function_name_index = name.rfind("::").map(|index| index + 2).unwrap_or(0);

        &name[function_name_index..]
    }};
}
pub use current_function;

/// Returns the path of the function that called this macro.
#[macro_export]
macro_rules! current_function_path {
    () => {{
        fn f() {}

        std::path::PathBuf::from($crate::test::format_current_function_path(f))
    }};
}
pub use current_function_path;

#[cfg(test)]
mod utils {
    use std::path::Path;

    use super::*;

    #[test]
    fn test_current_function_extract_function_name() {
        let name = current_function!();

        assert_eq!("test_current_function_extract_function_name", name);
    }

    #[tokio::test]
    async fn test_current_function_extract_async_function_name() {
        let name = current_function!();

        assert_eq!("test_current_function_extract_async_function_name", name);
    }

    #[test]
    fn test_format_function_path_from_given_function() {
        assert_eq!(
            "mithril_common/test_utils/utils/test_format_function_path_from_given_function",
            format_current_function_path(test_format_function_path_from_given_function)
        );
    }

    #[test]
    fn test_format_function_path_from_given_pseudo_function_f() {
        fn f() {}
        assert_eq!(
            "mithril_common/test_utils/utils/test_format_function_path_from_given_pseudo_function_f",
            format_current_function_path(f)
        );
    }

    #[tokio::test]
    async fn test_format_function_path_from_given_async_function_f() {
        fn f() {}
        assert_eq!(
            "mithril_common/test_utils/utils/test_format_function_path_from_given_async_function_f",
            format_current_function_path(f)
        );
    }

    #[test]
    fn test_build_current_function_path_using_macros() {
        assert_eq!(
            Path::new("mithril_common")
                .join("test_utils")
                .join("utils")
                .join("test_build_current_function_path_using_macros"),
            current_function_path!()
        );
    }

    #[tokio::test]
    async fn test_build_current_async_function_path_using_macros() {
        assert_eq!(
            Path::new("mithril_common")
                .join("test_utils")
                .join("utils")
                .join("test_build_current_async_function_path_using_macros"),
            current_function_path!()
        );
    }
}
