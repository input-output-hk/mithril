use std::fmt;
use std::marker::PhantomData;
use std::str::FromStr;

use serde::de::{Error, MapAccess, Visitor};
use serde::{Deserialize, Deserializer, de};

/// Deserialize a value that can be either a string or a struct if it implements `FromStr`.
///
/// Useful for deserializing environment variables that can be passed as a JSON string or as a map.
/// Ie: for the following enum:
/// ```rust
/// # use std::str::FromStr;
///
/// #[derive(Debug, serde::Deserialize)]
/// #[serde(tag = "type", rename_all = "lowercase")]
/// enum EnumParam {
///    Foo { foo: String },
/// }
///
/// impl FromStr for EnumParam {
///   type Err = serde_json::Error;
///
///   fn from_str(s: &str) -> Result<Self, Self::Err> {
///       serde_json::from_str(s)
///   }
/// }
/// ```
/// this allows `config` to deserialize the following environment variables:
/// ```shell
/// export ENUM_PARAM='{"type":"foo", "foo": "foo value"}'
/// ```
/// or
/// ```shell
/// export ENUM_PARAM__TYPE="foo"
/// export ENUM_PARAM__FOO="foo value"
/// ```
///
/// Code based on: <https://serde.rs/string-or-struct.html>
pub fn string_or_struct<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    T: Deserialize<'de> + FromStr,
    D: Deserializer<'de>,
    T::Err: fmt::Display,
{
    struct StringOrStruct<T>(PhantomData<fn() -> T>);

    impl<'de, T> Visitor<'de> for StringOrStruct<T>
    where
        T: Deserialize<'de> + FromStr,
        T::Err: fmt::Display,
    {
        type Value = T;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("string or map")
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            FromStr::from_str(value).map_err(Error::custom)
        }

        fn visit_map<M>(self, map: M) -> Result<Self::Value, M::Error>
        where
            M: MapAccess<'de>,
        {
            Deserialize::deserialize(de::value::MapAccessDeserializer::new(map))
        }
    }

    deserializer.deserialize_any(StringOrStruct(PhantomData))
}

/// Deserialize an optional value that can be either a string or a struct if it implements `FromStr`.
///
/// Important: to handle the case where the key is missing in the JSON file, the `#[serde(default)]` attribute must be set.
///
/// Useful for deserializing environment variables that can be passed as a JSON string or as a map.
/// Ie: for the following enum:
/// ```rust
/// # use std::str::FromStr;
/// use mithril_cli_helper::serde_deserialization::string_or_struct_optional;
///
/// #[derive(Debug, PartialEq, serde::Deserialize)]
/// #[serde(tag = "type", rename_all = "lowercase")]
/// enum EnumParam {
///    Foo { foo: String },
/// }
///
/// impl FromStr for EnumParam {
///   type Err = serde_json::Error;
///
///   fn from_str(s: &str) -> Result<Self, Self::Err> {
///       serde_json::from_str(s)
///   }
/// }
///
/// #[derive(Debug, PartialEq, serde::Deserialize)]
/// struct Config {
///     #[serde(default, deserialize_with = "string_or_struct_optional")]
///     enum_param: Option<EnumParam>,
/// }
///
/// // Deserialize from null
/// let config: Config = serde_json::from_str(r#"{"enum_param": null}"#).unwrap();
/// assert_eq!(config.enum_param, None);
///
/// // Deserialize from missing key
/// let config: Config = serde_json::from_str(r#"{}"#).unwrap();
/// assert_eq!(config.enum_param, None);
///
/// // Deserialize from object
/// let config: Config = serde_json::from_str(r#"{"enum_param": {"type":"foo", "foo": "bar"}}"#).unwrap();
/// assert_eq!(config.enum_param, Some(EnumParam::Foo { foo: "bar".to_string() }));
/// ```
///
/// this allows `config` to deserialize the following environment variables:
/// ```shell
/// export ENUM_PARAM='{"type":"foo", "foo": "foo value"}'
/// ```
/// or
/// ```shell
/// export ENUM_PARAM__TYPE="foo"
/// export ENUM_PARAM__FOO="foo value"
/// ```
///
/// Code based on: <https://serde.rs/string-or-struct.html>
pub fn string_or_struct_optional<'de, T, D>(deserializer: D) -> Result<Option<T>, D::Error>
where
    T: Deserialize<'de> + FromStr + 'de,
    D: Deserializer<'de>,
    T::Err: fmt::Display,
{
    struct OptionalStringOrStruct<T>(PhantomData<fn() -> T>);

    impl<'de, T> Visitor<'de> for OptionalStringOrStruct<T>
    where
        T: Deserialize<'de> + FromStr,
        T::Err: fmt::Display,
    {
        type Value = Option<T>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("null, string, or map")
        }

        fn visit_none<E>(self) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(None)
        }

        fn visit_unit<E>(self) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(None)
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: Error,
        {
            FromStr::from_str(value).map(Some).map_err(Error::custom)
        }

        fn visit_map<M>(self, map: M) -> Result<Self::Value, M::Error>
        where
            M: MapAccess<'de>,
        {
            Deserialize::deserialize(de::value::MapAccessDeserializer::new(map)).map(Some)
        }
    }

    deserializer.deserialize_any(OptionalStringOrStruct(PhantomData))
}

#[cfg(test)]
mod tests {
    use config::Source;
    use std::collections::HashMap;

    use super::*;

    #[derive(Debug, PartialEq, Deserialize)]
    #[serde(tag = "type", rename_all = "lowercase")]
    enum InternallyTaggedEnumParam {
        Foo { foo: String },
        Bar { bar: u16 },
    }

    impl FromStr for InternallyTaggedEnumParam {
        type Err = serde_json::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            serde_json::from_str(s)
        }
    }

    fn read_config_from_source<T, U>(source: T) -> U
    where
        T: Source + Send + Sync + 'static,
        U: de::DeserializeOwned,
    {
        config::Config::builder()
            .add_source(source)
            .build()
            .unwrap()
            .try_deserialize()
            .unwrap()
    }

    mod string_or_struct {
        use super::*;

        #[derive(Debug, PartialEq, Deserialize)]
        struct TestSettings {
            #[serde(deserialize_with = "string_or_struct")]
            enum_param: InternallyTaggedEnumParam,
        }

        #[test]
        fn deserialize_with_serde_json_map_value() {
            let json = r#"{"enum_param": {"type":"foo", "foo": "foo value"}}"#;

            let config: TestSettings = serde_json::from_str(json).unwrap();

            assert_eq!(
                config.enum_param,
                InternallyTaggedEnumParam::Foo {
                    foo: "foo value".to_string()
                }
            );
        }

        #[test]
        fn deserialize_with_serde_json_string_value() {
            let json = r#"{"enum_param": "{\"type\":\"bar\", \"bar\": 123}"}"#;

            let config: TestSettings = serde_json::from_str(json).unwrap();

            assert_eq!(
                config.enum_param,
                InternallyTaggedEnumParam::Bar { bar: 123 }
            );
        }

        #[test]
        fn deserialize_fails_with_invalid_string_content() {
            let json = r#"{"enum_param": "not a json string"}"#;

            let result: Result<TestSettings, _> = serde_json::from_str(json);

            assert!(result.is_err());
        }

        #[test]
        fn deserialize_fails_with_invalid_map_structure() {
            let json = r#"{"enum_param": {"type":"unknown", "data": 123}}"#;

            let result: Result<TestSettings, _> = serde_json::from_str(json);

            assert!(result.is_err());
        }

        #[test]
        fn deserialize_config_from_single_string_in_environment_vars() {
            let single_string_environment = HashMap::from([(
                "ENUM_PARAM".to_string(),
                r#"{"type":"foo", "foo": "foo value"}"#.to_string(),
            )]);

            let config: TestSettings = read_config_from_source(
                config::Environment::default()
                    .source(Some(single_string_environment))
                    .separator("__"),
            );

            assert_eq!(
                config.enum_param,
                InternallyTaggedEnumParam::Foo {
                    foo: "foo value".to_string()
                }
            );
        }

        #[test]
        fn deserialize_config_from_map_of_strings_in_environment_vars() {
            let map_environment = HashMap::from([
                ("ENUM_PARAM__TYPE".to_string(), "foo".to_string()),
                ("ENUM_PARAM__FOO".to_string(), "foo value".to_string()),
            ]);

            let config: TestSettings = read_config_from_source(
                config::Environment::default()
                    .source(Some(map_environment))
                    .separator("__"),
            );

            assert_eq!(
                config.enum_param,
                InternallyTaggedEnumParam::Foo {
                    foo: "foo value".to_string()
                }
            );
        }

        #[test]
        fn deserialize_config_from_object_in_json_file() {
            let json = r#"{"enum_param": {"type":"bar", "bar": 23452} }"#.to_string();

            let config: TestSettings =
                read_config_from_source(config::File::from_str(&json, config::FileFormat::Json));

            assert_eq!(
                config.enum_param,
                InternallyTaggedEnumParam::Bar { bar: 23452 }
            );
        }

        #[test]
        fn deserialize_config_from_escaped_json_string_in_json_file() {
            let json = r#"{"enum_param": "{\"type\":\"bar\", \"bar\": 23452}" }"#;

            let config: TestSettings =
                read_config_from_source(config::File::from_str(json, config::FileFormat::Json));

            assert_eq!(
                config.enum_param,
                InternallyTaggedEnumParam::Bar { bar: 23452 }
            );
        }
    }

    mod string_or_struct_optional {
        use super::*;

        #[derive(Debug, PartialEq, Deserialize)]
        struct TestOptSettings {
            #[serde(default, deserialize_with = "string_or_struct_optional")]
            enum_param: Option<InternallyTaggedEnumParam>,
        }

        #[test]
        fn deserialize_config_with_null_value_in_json_file() {
            let json = r#"{"enum_param": null}"#;

            let config: TestOptSettings =
                read_config_from_source(config::File::from_str(json, config::FileFormat::Json));
            assert_eq!(config.enum_param, None);
        }

        #[test]
        fn deserialize_with_serde_json_null_value() {
            let json = r#"{"enum_param": null}"#;

            let config: TestOptSettings = serde_json::from_str(json).unwrap();
            assert_eq!(config.enum_param, None);
        }

        #[test]
        fn deserialize_with_missing_key_in_json() {
            let json = r#"{}"#;

            let config: TestOptSettings = serde_json::from_str(json).unwrap();
            assert_eq!(config.enum_param, None);
        }

        #[test]
        fn deserialize_fails_with_invalid_string_content() {
            let json = r#"{"enum_param": "not a json string"}"#;

            let result: Result<TestOptSettings, _> = serde_json::from_str(json);

            assert!(result.is_err());
        }

        #[test]
        fn deserialize_fails_with_invalid_map_structure() {
            let json = r#"{"enum_param": {"type":"unknown", "data": 123}}"#;

            let result: Result<TestOptSettings, _> = serde_json::from_str(json);

            assert!(result.is_err());
        }

        #[test]
        fn deserialize_config_from_single_string_in_environment_vars() {
            let single_string_environment = HashMap::from([(
                "ENUM_PARAM".to_string(),
                r#"{"type":"foo", "foo": "foo value"}"#.to_string(),
            )]);

            let config: TestOptSettings = read_config_from_source(
                config::Environment::default()
                    .source(Some(single_string_environment))
                    .separator("__"),
            );

            assert_eq!(
                config.enum_param,
                Some(InternallyTaggedEnumParam::Foo {
                    foo: "foo value".to_string()
                })
            );
        }

        #[test]
        fn deserialize_config_from_map_of_strings_in_environment_vars() {
            let map_environment = HashMap::from([
                ("ENUM_PARAM__TYPE".to_string(), "foo".to_string()),
                ("ENUM_PARAM__FOO".to_string(), "foo value".to_string()),
            ]);

            let config: TestOptSettings = read_config_from_source(
                config::Environment::default()
                    .source(Some(map_environment))
                    .separator("__"),
            );

            assert_eq!(
                config.enum_param,
                Some(InternallyTaggedEnumParam::Foo {
                    foo: "foo value".to_string()
                })
            );
        }

        #[test]
        fn deserialize_config_from_object_in_json_file() {
            let json = r#"{"enum_param": {"type":"bar", "bar": 23452} }"#.to_string();

            let config: TestOptSettings =
                read_config_from_source(config::File::from_str(&json, config::FileFormat::Json));

            assert_eq!(
                config.enum_param,
                Some(InternallyTaggedEnumParam::Bar { bar: 23452 })
            );
        }

        #[test]
        fn deserialize_config_from_escaped_json_string_in_json_file() {
            let json = r#"{"enum_param": "{\"type\":\"bar\", \"bar\": 23452}" }"#;

            let config: TestOptSettings =
                read_config_from_source(config::File::from_str(json, config::FileFormat::Json));

            assert_eq!(
                config.enum_param,
                Some(InternallyTaggedEnumParam::Bar { bar: 23452 })
            );
        }
    }
}
