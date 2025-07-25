//! A set of tools for working with `automock`
//!
//! IMPORTANT: To avoid polluting production code, those tools do not expose or reexpose any
//! `automock`types, users need to add them themselves to their crates.
use std::sync::Arc;

/// Helper to create configured Mockall mock.
///
/// This allows creation of the mock in a dedicated block isolated from the remaining test method
/// code.
pub struct MockBuilder<M: Default> {
    phantom: std::marker::PhantomData<M>,
}

impl<M: Default> MockBuilder<M> {
    /// Create a new instance of the mock with the given configuration
    ///
    /// The type must be specified either:
    /// ```
    /// use mithril_common::test::mock_extensions::MockBuilder;
    /// # #[derive(Default)] struct MockType {};
    ///
    /// // from the builder generic
    /// let mock = MockBuilder::<MockType>::configure(|mock| {});
    ///
    /// // or from the closure parameter
    /// let mock = MockBuilder::configure(|mock: &mut MockType| {});
    /// ```
    pub fn configure(mock_config: impl FnOnce(&mut M)) -> Arc<M> {
        let mut mock = M::default();
        mock_config(&mut mock);
        Arc::new(mock)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[mockall::automock]
    trait TestTrait {
        fn test_method(&self) -> String;
    }

    #[test]
    fn using_mock_builder() {
        // specify the type on the closure parameter
        let mock = MockBuilder::configure(|mock: &mut MockTestTrait| {
            mock.expect_test_method()
                .returning(|| "test explicit type".to_string());
        });
        assert_eq!("test explicit type".to_string(), mock.test_method());

        // specify the type on the builder generic
        let mock = MockBuilder::<MockTestTrait>::configure(|mock| {
            mock.expect_test_method().returning(|| "test turbofish".to_string());
        });
        assert_eq!("test turbofish".to_string(), mock.test_method());
    }
}
