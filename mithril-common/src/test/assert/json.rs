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
