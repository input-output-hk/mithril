/// Assert that two JSON values are equal, ignoring object key order.
///
/// Each argument must be prefixed with its input kind:
///
/// - `json:` for a string containing JSON data.
/// - `value:` for a Rust value that implements [`serde::Serialize`].
///
/// The macro parses `json:` arguments as [`serde_json::Value`] and serializes
/// `value:` arguments to [`serde_json::Value`] before comparing them. Since
/// comparison is done on `serde_json::Value`, JSON object key order does not
/// matter.
///
/// # Examples
///
/// ```rust
/// # use mithril_common::assert_same_json;
/// # use serde::Serialize;
/// assert_same_json!(
///     json: r#"{"foo": 1, "bar": 2}"#,
///     json: r#"{"bar": 2, "foo": 1}"#
/// );
///
/// #[derive(Serialize)]
/// struct Payload {
///     foo: String,
///     bar: u64,
/// }
///
/// assert_same_json!(
///     json: r#"{"foo": "hello", "bar": 42}"#,
///     value: Payload {
///         foo: "hello".to_string(),
///         bar: 42,
///     }
/// );
/// ```
#[macro_export]
macro_rules! assert_same_json {
    (json: $expected:expr, json: $actual:expr ) => {{
        $crate::assert_same_json!(@compare
            $crate::assert_same_json!(@from_json $expected, "expected"),
            $crate::assert_same_json!(@from_json $actual, "actual")
        );
    }};
    (value: $expected:expr, json: $actual:expr ) => {{
        $crate::assert_same_json!(@compare
            $crate::assert_same_json!(@from_value $expected, "expected"),
            $crate::assert_same_json!(@from_json $actual, "actual")
        );
    }};
    ( json: $expected:expr, value: $actual:expr ) => {{
        $crate::assert_same_json!(@compare
            $crate::assert_same_json!(@from_json $expected, "expected"),
            $crate::assert_same_json!(@from_value $actual, "actual")
        );
    }};
    ( value: $expected:expr, value: $actual:expr ) => {{
        $crate::assert_same_json!(@compare
            $crate::assert_same_json!(@from_value $expected, "expected"),
            $crate::assert_same_json!(@from_value $actual, "actual")
        );
    }};

    // ------------------------ Internal helpers ------------------------
    (@from_json $json:expr, $kind:literal) => {
        serde_json::from_str::<serde_json::Value>($json)
            .unwrap_or_else(|error| {
                panic!(
                    "failed to parse {} JSON expression `{}`: {error}",
                    $kind,
                    stringify!($json)
                )
            })
    };

    (@from_value $value:expr, $kind:literal) => {
        serde_json::to_value(&$value)
            .unwrap_or_else(|error| {
                panic!(
                    "{} value should serialize to JSON: {error}",
                    $kind,
                )
            })
    };

    (@compare $expected:expr, $actual:expr) => {{
        let expected_json_value = $expected;
        let actual_json_value = $actual;

        assert_eq!(expected_json_value, actual_json_value, "JSON values differ");
    }};
}
pub use assert_same_json;

#[cfg(test)]
mod tests {
    use serde::Serialize;

    #[derive(Debug, PartialEq, Serialize)]
    struct TestStruct {
        foo: String,
        bar: u64,
    }

    struct FailingSerialize;

    impl Serialize for FailingSerialize {
        fn serialize<S>(&self, _serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            Err(serde::ser::Error::custom(
                "serialization failed as expected",
            ))
        }
    }

    #[test]
    fn compare_strings() {
        assert_same_json!(json: r#""foo""#, json: r#""foo""#);
        assert_same_json!(json: r#"["foo", "bar"]"#, json: r#"["foo", "bar"]"#);
        assert_same_json!(json: r#"{"foo": "1", "bar": 2}"#, json: r#"{"bar": 2, "foo": "1"}"#);
    }

    #[test]
    #[should_panic(expected = "JSON values differ")]
    fn compare_fail() {
        assert_same_json!(json: r#"{"foo": "1", "bar": 2}"#, json: r#"{"pika": 2, "foo": "1"}"#);
    }

    #[test]
    #[should_panic(
        expected = "failed to parse expected JSON expression `r#\"{\"foo\": \"1\", \"bar\": 2\"#`: "
    )]
    fn fail_on_parsing_json() {
        assert_same_json!(json: r#"{"foo": "1", "bar": 2"#, json: r#"{"pika": 2, "foo": "1"}"#);
    }

    #[test]
    #[should_panic(expected = "actual value should serialize to JSON")]
    fn fail_on_serializing_value() {
        assert_same_json!(
            json: r#"{"foo": "bar"}"#,
            value: FailingSerialize
        );
    }

    #[test]
    fn compare_serializable_symbols() {
        assert_same_json!(
            value: TestStruct {
                foo: "foo".to_string(),
                bar: 1
            },
            value: TestStruct {
                foo: "foo".to_string(),
                bar: 1
            }
        );
    }

    #[test]
    fn compare_string_to_serializable_symbols() {
        assert_same_json!(
            json: r#"{"bar": 1 ,"foo": "foo"}"#,
            value: TestStruct {
                foo: "foo".to_string(),
                bar: 1
            }
        );
        assert_same_json!(
            value: TestStruct {
                foo: "foo".to_string(),
                bar: 1
            },
            json: r#"{"bar": 1 ,"foo": "foo"}"#
        );
    }
}
