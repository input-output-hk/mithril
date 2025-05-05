//! Test utilities
//!
//! They contains:
//! * A Open Api Spec tester
//! * Some precomputed fake data and keys
//! * A builder of [MithrilFixture] to generate signers alongside a stake distribution
//!

#[cfg(feature = "apispec")]
#[cfg_attr(docsrs, doc(cfg(feature = "apispec")))]
pub mod apispec;

pub mod fake_data;
pub mod fake_keys;

mod cardano_transactions_builder;
mod certificate_chain_builder;
mod dir_eq;
mod fixture_builder;
mod memory_logger;
mod mithril_fixture;
mod precomputed_kes_key;
mod temp_dir;

#[cfg(feature = "test_http_server")]
#[cfg_attr(docsrs, doc(cfg(feature = "test_http_server")))]
pub mod test_http_server;

pub use cardano_transactions_builder::CardanoTransactionsBuilder;
pub use certificate_chain_builder::{
    CertificateChainBuilder, CertificateChainBuilderContext, CertificateChainingMethod,
};
pub use dir_eq::*;
pub use fixture_builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
pub use memory_logger::*;
pub use mithril_fixture::{MithrilFixture, SignerFixture};
pub use temp_dir::*;
#[cfg(test)]
pub(crate) use utils::*;

/// Compare two json strings ignoring keys order
#[macro_export]
macro_rules! assert_same_json {
    ( $expected:expr, $actual:expr ) => {
        assert_eq!(
            serde_json::from_str::<serde_json::Value>($expected).unwrap(),
            serde_json::from_str::<serde_json::Value>($actual).unwrap()
        )
    };
}
pub use assert_same_json;

/// Compare two iterators ignoring the order
pub fn equivalent_to<T, I1, I2>(a: I1, b: I2) -> bool
where
    T: PartialEq + Ord,
    I1: IntoIterator<Item = T> + Clone,
    I2: IntoIterator<Item = T> + Clone,
{
    let a = as_sorted_vec(a);
    let b = as_sorted_vec(b);
    a == b
}

/// Assert that two iterators are equivalent
pub fn assert_equivalent<T, I1, I2>(a: I1, b: I2)
where
    T: PartialEq + Ord + std::fmt::Debug,
    I1: IntoIterator<Item = T> + Clone,
    I2: IntoIterator<Item = T> + Clone,
{
    let a = as_sorted_vec(a);
    let b = as_sorted_vec(b);
    assert_eq!(a, b);
}

/// Assert that two iterators are equivalent
#[macro_export]
macro_rules! assert_equivalent_macro {
    ( $expected:expr, $actual:expr ) => {{
        let expected = $crate::test_utils::as_sorted_vec($expected);
        let actual = $crate::test_utils::as_sorted_vec($actual);
        assert_eq!(expected, actual);
    }};
}
pub use assert_equivalent_macro;

/// Create a sorted clone af an iterable.
pub fn as_sorted_vec<T: Ord, I: IntoIterator<Item = T> + Clone>(iter: I) -> Vec<T> {
    let mut list: Vec<T> = iter.clone().into_iter().collect();
    list.sort();
    list
}

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
        let name = $crate::test_utils::format_current_function_module(f);
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

        std::path::PathBuf::from($crate::test_utils::format_current_function_path(f))
    }};
}
pub use current_function_path;

#[cfg(test)]
mod utils {
    use std::io;
    use std::sync::Arc;
    use std::{collections::HashSet, path::Path};

    use slog::{Drain, Logger};
    use slog_async::Async;
    use slog_term::{CompactFormat, PlainDecorator};

    use super::*;

    pub struct TestLogger;

    #[cfg(test)]
    impl TestLogger {
        fn from_writer<W: io::Write + Send + 'static>(writer: W) -> Logger {
            let decorator = PlainDecorator::new(writer);
            let drain = CompactFormat::new(decorator).build().fuse();
            let drain = Async::new(drain).build().fuse();
            Logger::root(Arc::new(drain), slog::o!())
        }

        pub fn stdout() -> Logger {
            Self::from_writer(slog_term::TestStdoutWriter)
        }

        pub fn memory() -> (Logger, MemoryDrainForTestInspector) {
            let (drain, inspector) = MemoryDrainForTest::new();
            (Logger::root(drain.fuse(), slog::o!()), inspector)
        }
    }

    #[test]
    fn test_equivalent_to() {
        assert!(equivalent_to(vec![1, 2, 3], vec![3, 2, 1]));
        assert!(equivalent_to(vec![1, 2, 3], vec![2, 1, 3]));
        assert!(!equivalent_to(vec![1, 2, 3], vec![3, 2, 1, 4]));
        assert!(!equivalent_to(vec![1, 2, 3], vec![3, 2]));

        assert!(equivalent_to([1, 2, 3], vec![3, 2, 1]));
        assert!(equivalent_to(&[1, 2, 3], &vec![3, 2, 1]));
        assert!(equivalent_to([1, 2, 3], HashSet::from([3, 2, 1])));
        assert!(equivalent_to(vec![1, 2, 3], HashSet::from([3, 2, 1])));
        assert!(equivalent_to(&vec![1, 2, 3], &HashSet::from([3, 2, 1])));

        assert_equivalent(vec![1, 2, 3], vec![3, 2, 1]);
        assert_equivalent(vec![1, 2, 3], vec![2, 1, 3]);

        assert_equivalent([1, 2, 3], vec![3, 2, 1]);
        assert_equivalent(&[1, 2, 3], &vec![3, 2, 1]);
        assert_equivalent([1, 2, 3], HashSet::from([3, 2, 1]));
        assert_equivalent(vec![1, 2, 3], HashSet::from([3, 2, 1]));
        assert_equivalent(&vec![1, 2, 3], &HashSet::from([3, 2, 1]));
    }

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
