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
mod fixture_builder;
mod mithril_fixture;

mod temp_dir;

#[cfg(feature = "test_http_server")]
#[cfg_attr(docsrs, doc(cfg(feature = "test_http_server")))]
pub mod test_http_server;

pub use cardano_transactions_builder::CardanoTransactionsBuilder;
pub use fixture_builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};
pub use mithril_fixture::{MithrilFixture, SignerFixture};
pub use temp_dir::*;
#[cfg(test)]
pub use utils::*;

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

fn as_sorted_vec<T: Ord, I: IntoIterator<Item = T> + Clone>(iter: I) -> Vec<T> {
    let mut list: Vec<T> = iter.clone().into_iter().collect();
    list.sort();
    list
}

pub use assert_same_json;

#[cfg(test)]
mod utils {
    use std::collections::HashSet;
    use std::fs::File;
    use std::io;
    use std::sync::Arc;

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

        pub fn file(filepath: &std::path::Path) -> Logger {
            Self::from_writer(File::create(filepath).unwrap())
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
}
