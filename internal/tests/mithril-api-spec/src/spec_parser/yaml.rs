use saphyr::{Scalar, Yaml};
use serde_json::Value;

/// Converts a [saphyr::Yaml][Yaml] value to a [serde_json::Value][Value].
///
/// This is a crude, minimal, implementation aimed only at handling `mithril-api-spec` needs of
/// parsing `openapi.yaml` files.
/// It should not be reused as-is for other cases.
pub(crate) fn convert_yaml_to_serde_json(yaml_value: &Yaml) -> Result<Value, String> {
    match yaml_value {
        Yaml::Value(scalar) => match scalar {
            Scalar::String(s) => Ok(Value::String(s.to_string())),
            Scalar::Integer(i) => Ok(Value::from(*i)),
            Scalar::FloatingPoint(f) => {
                serde_json::Number::from_f64(**f).map(Value::Number).ok_or_else(|| {
                    format!(
                        "Could not convert saphyr::Yaml::FloatingPoint {} to JSON number",
                        f
                    )
                })
            }
            Scalar::Boolean(b) => Ok(Value::Bool(*b)),
            Scalar::Null => Ok(Value::Null),
        },
        Yaml::Sequence(seq) => {
            let mut json_array = Vec::new();
            for item in seq {
                json_array.push(convert_yaml_to_serde_json(item)?);
            }
            Ok(Value::Array(json_array))
        }
        Yaml::Mapping(map) => {
            let mut json_obj = serde_json::Map::new();
            for (key, value) in map {
                let key_str = match key {
                    Yaml::Value(Scalar::String(s)) => s,
                    _ => return Err("saphyr::Yaml::Mapping key must be a string".to_string()),
                };
                json_obj.insert(key_str.to_string(), convert_yaml_to_serde_json(value)?);
            }
            Ok(Value::Object(json_obj))
        }
        Yaml::Representation(_val, _scalar_style, _tag) => {
            Err("saphyr::Yaml::Representation variant is not supported".to_string())
        }
        Yaml::Tagged(_, boxed) => convert_yaml_to_serde_json(boxed),
        Yaml::Alias(_) => Err("saphyr::Yaml::Alias are not supported".to_string()),
        Yaml::BadValue => Err("Bad YAML value".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use saphyr::LoadableYamlNode;

    use super::*;

    #[test]
    fn test_convert_scalar_string() {
        let yaml = Yaml::Value(Scalar::String("test".into()));
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        assert_eq!(json, Value::String("test".to_string()));
    }

    #[test]
    fn test_convert_scalar_int() {
        let yaml = Yaml::Value(Scalar::Integer(42));
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        assert_eq!(json, Value::Number(42.into()));
    }

    #[test]
    fn test_convert_scalar_float() {
        let yaml = Yaml::load_from_str("2.14").unwrap()[0].to_owned();
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        assert!(json.is_number());
        assert_eq!(json.as_f64().unwrap(), 2.14);
    }

    #[test]
    fn test_convert_scalar_bool() {
        let yaml = Yaml::Value(Scalar::Boolean(true));
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        assert_eq!(json, Value::Bool(true));
    }

    #[test]
    fn test_convert_scalar_null() {
        let yaml = Yaml::Value(Scalar::Null);
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        assert_eq!(json, Value::Null);
    }

    #[test]
    fn test_convert_sequence() {
        let yaml = Yaml::Sequence(vec![
            Yaml::Value(Scalar::Integer(1)),
            Yaml::Value(Scalar::Integer(2)),
        ]);
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        assert_eq!(
            json,
            Value::Array(vec![Value::Number(1.into()), Value::Number(2.into())])
        );
    }

    #[test]
    fn test_convert_mapping() {
        let yaml = Yaml::load_from_str("key: value").unwrap()[0].to_owned();
        let json = convert_yaml_to_serde_json(&yaml).unwrap();
        let mut expected = serde_json::Map::new();
        expected.insert("key".to_string(), Value::String("value".to_string()));
        assert_eq!(json, Value::Object(expected));
    }

    #[test]
    fn test_invalid_mapping_key() {
        let yaml = Yaml::load_from_str("42: value").unwrap()[0].to_owned();
        assert_eq!(
            convert_yaml_to_serde_json(&yaml).unwrap_err(),
            "saphyr::Yaml::Mapping key must be a string"
        );
    }

    #[test]
    fn test_representation_is_not_supported() {
        let yaml = Yaml::Representation("test".into(), saphyr::ScalarStyle::SingleQuoted, None);
        let error = convert_yaml_to_serde_json(&yaml).unwrap_err();
        assert_eq!(
            error,
            "saphyr::Yaml::Representation variant is not supported"
        );
    }

    #[test]
    fn test_alias_is_not_supported() {
        let yaml = Yaml::Alias(1);
        let error = convert_yaml_to_serde_json(&yaml).unwrap_err();
        assert_eq!(error, "saphyr::Yaml::Alias are not supported");
    }

    #[test]
    fn test_bad_value_is_not_supported() {
        let yaml = Yaml::BadValue;
        let error = convert_yaml_to_serde_json(&yaml).unwrap_err();
        assert_eq!(error, "Bad YAML value");
    }
}
