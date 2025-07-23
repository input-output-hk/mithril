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
        let expected = $crate::test::as_sorted_vec($expected);
        let actual = $crate::test::as_sorted_vec($actual);
        assert_eq!(expected, actual);
    }};
}
pub use assert_equivalent_macro;

/// Create a sorted clone of an iterable.
pub fn as_sorted_vec<T: Ord, I: IntoIterator<Item = T> + Clone>(iter: I) -> Vec<T> {
    let mut list: Vec<T> = iter.clone().into_iter().collect();
    list.sort();
    list
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

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
