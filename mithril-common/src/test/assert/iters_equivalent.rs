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
#[macro_export]
macro_rules! assert_equivalent {
    ($expected:expr, $actual:expr $(,)?) => {{
        let expected = $expected;
        let actual = $actual;
        assert_eq!(
            $crate::test::as_sorted_vec(expected),
            $crate::test::as_sorted_vec(actual)
        );
    }};
}
pub use assert_equivalent;

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

        assert_equivalent!(vec![1, 2, 3], vec![3, 2, 1]);
        assert_equivalent!(vec![1, 2, 3], vec![2, 1, 3]);

        assert_equivalent!([1, 2, 3], vec![3, 2, 1]);
        assert_equivalent!(&[1, 2, 3], &vec![3, 2, 1]);
        assert_equivalent!([1, 2, 3], HashSet::from([3, 2, 1]));
        assert_equivalent!(vec![1, 2, 3], HashSet::from([3, 2, 1]));
        assert_equivalent!(&vec![1, 2, 3], &HashSet::from([3, 2, 1]));
    }
}
