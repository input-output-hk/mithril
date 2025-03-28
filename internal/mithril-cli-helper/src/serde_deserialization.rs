use std::fmt;
use std::marker::PhantomData;
use std::str::FromStr;

use serde::de::{Error, MapAccess, Visitor};
use serde::{de, Deserialize, Deserializer};

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

        fn visit_str<E>(self, value: &str) -> Result<T, E>
        where
            E: Error,
        {
            FromStr::from_str(value).map_err(Error::custom)
        }

        fn visit_map<M>(self, map: M) -> Result<T, M::Error>
        where
            M: MapAccess<'de>,
        {
            Deserialize::deserialize(de::value::MapAccessDeserializer::new(map))
        }
    }

    deserializer.deserialize_any(StringOrStruct(PhantomData))
}

#[cfg(test)]
mod tests {
    use config::Source;
    use std::collections::HashMap;

    use super::*;

    #[derive(Debug, PartialEq, Deserialize)]
    struct TestSettings {
        #[serde(deserialize_with = "string_or_struct")]
        enum_param: InternallyTaggedEnumParam,
    }

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

    fn read_config_from_source<T>(source: T) -> TestSettings
    where
        T: Source + Send + Sync + 'static,
    {
        config::Config::builder()
            .add_source(source)
            .build()
            .unwrap()
            .try_deserialize()
            .unwrap()
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
