//! Utilities to register config parameters.

/// Register an optional parameter in the config map when it's not None.
#[macro_export]
macro_rules! register_config_value_option {
    ( $map:ident, $namespace:expr_2021, $self:ident.$command:ident ) => {{
        if let Some(value) = $self.$command.clone() {
            register_config_value!($map, $namespace, $command = value);
        }
    }};
    ( $map:ident, $namespace:expr_2021, $self:ident.$command:ident, $mapping:expr_2021 ) => {{
        if let Some(value) = $self.$command.clone() {
            register_config_value!($map, $namespace, $command = $mapping(value));
        }
    }};
}

/// Register a boolean parameter in the config map only when it's true.
#[macro_export]
macro_rules! register_config_value_bool {
    ( $map:ident, $namespace:expr_2021, $self:ident.$command:ident ) => {{
        if $self.$command {
            register_config_value!($map, $namespace, $command = true);
        }
    }};
}

/// Register a parameter in the config map using the identifier as key.
/// Example:
///     register_config_value!(map, namespace, self.identifier)
///
/// The same macro, with a different syntax, is used to insert the given value without transformation.
/// Iit is designed to be used by other macros.
/// Example:
///     register_config_value!(map, namespace, identifier = value)
#[macro_export]
macro_rules! register_config_value {
    ( $map:ident, $namespace:expr_2021, $self:ident.$command:ident ) => {{
        register_config_value!($map, $namespace, $command = $self.$command);
    }};
    ( $map:ident, $namespace:expr_2021, $self:ident.$command:ident, $mapping:expr_2021 ) => {{
        register_config_value!($map, $namespace, $command = $mapping($self.$command));
    }};

    ( $map:ident, $namespace:expr_2021, $command:ident = $value:expr_2021 ) => {{
        $map.insert(
            stringify!($command).to_string(),
            config::Value::new(Some($namespace), $value),
        );
    }};
}

#[cfg(test)]
mod tests {
    use config::{ConfigError, Map, Source, Value, ValueKind};
    use std::collections::HashMap;

    #[test]
    fn test_register_config_value_macro_with_the_value() {
        let mut map = HashMap::new();
        register_config_value!(
            map,
            &"namespace".to_string(),
            server_ip = Some("value_server_ip".to_string())
        );

        let expected = HashMap::from([(
            "server_ip".to_string(),
            Value::new(
                Some(&"namespace".to_string()),
                ValueKind::from("value_server_ip"),
            ),
        )]);

        assert_eq!(expected, map);
    }

    #[derive(Debug, Clone)]
    pub enum EnumValue {
        Test,
    }
    impl From<EnumValue> for ValueKind {
        fn from(value: EnumValue) -> Self {
            match value {
                EnumValue::Test => ValueKind::String("Test".to_string()),
            }
        }
    }

    #[test]
    fn test_register_config_value_macro_add_value() {
        struct Fake {
            string_value: String,
            u64_value: u64,
            enum_value: EnumValue,
        }

        let fake = Fake {
            string_value: "a string value".to_string(),
            u64_value: 124,
            enum_value: EnumValue::Test,
        };

        let mut map = HashMap::new();
        register_config_value!(map, &"namespace".to_string(), fake.string_value);
        register_config_value!(map, &"namespace".to_string(), fake.u64_value);
        register_config_value!(map, &"namespace".to_string(), fake.enum_value);

        let expected = HashMap::from([
            (
                "string_value".to_string(),
                Value::new(
                    Some(&"namespace".to_string()),
                    ValueKind::from("a string value"),
                ),
            ),
            (
                "u64_value".to_string(),
                Value::new(Some(&"namespace".to_string()), ValueKind::from(124_u64)),
            ),
            (
                "enum_value".to_string(),
                Value::new(Some(&"namespace".to_string()), ValueKind::from("Test")),
            ),
        ]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_config_value_macro_with_mapping_transform_value_before_adding_it() {
        struct Fake {
            string_value: String,
        }

        let fake = Fake {
            string_value: "a string value".to_string(),
        };

        let mut map = HashMap::new();
        register_config_value!(
            map,
            &"namespace".to_string(),
            fake.string_value,
            |v: String| { format!("mapped_value from {v}") }
        );

        let expected = HashMap::from([(
            "string_value".to_string(),
            Value::new(
                Some(&"namespace".to_string()),
                ValueKind::from("mapped_value from a string value"),
            ),
        )]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_config_value_option_macro_not_add_none_value() {
        struct Fake {
            option_with_value: Option<String>,
            option_none: Option<String>,
        }

        let fake = Fake {
            option_with_value: Some("value_of_option".to_string()),
            option_none: None,
        };

        let mut map = HashMap::new();
        register_config_value_option!(map, &"namespace".to_string(), fake.option_with_value);
        register_config_value_option!(map, &"namespace".to_string(), fake.option_none);

        let expected = HashMap::from([(
            "option_with_value".to_string(),
            Value::new(
                Some(&"namespace".to_string()),
                ValueKind::from("value_of_option"),
            ),
        )]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_config_value_option_macro_with_mapping_transform_value_before_adding_it() {
        struct Fake {
            option_with_value: Option<String>,
            option_none: Option<String>,
        }

        let fake = Fake {
            option_with_value: Some("value_of_option".to_string()),
            option_none: None,
        };

        let mut map = HashMap::new();
        register_config_value_option!(
            map,
            &"namespace".to_string(),
            fake.option_with_value,
            |v: String| format!("mapped_value from {v}")
        );
        register_config_value_option!(map, &"namespace".to_string(), fake.option_none);

        let expected = HashMap::from([(
            "option_with_value".to_string(),
            Value::new(
                Some(&"namespace".to_string()),
                ValueKind::from("mapped_value from value_of_option"),
            ),
        )]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_config_value_bool_macro_add_only_true_values() {
        struct Fake {
            bool_true: bool,
            bool_false: bool,
        }

        let fake = Fake {
            bool_true: true,
            bool_false: false,
        };

        let mut map = HashMap::new();
        register_config_value_bool!(map, &"namespace".to_string(), fake.bool_true);
        register_config_value_bool!(map, &"namespace".to_string(), fake.bool_false);

        let expected = HashMap::from([(
            "bool_true".to_string(),
            Value::new(Some(&"namespace".to_string()), ValueKind::from(true)),
        )]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_config_value_macro() {
        struct Fake {
            server_ip: Option<String>,
        }

        let fake = Fake {
            server_ip: Some("value_server_ip".to_string()),
        };

        let mut map = HashMap::new();
        register_config_value!(map, &"namespace".to_string(), fake.server_ip);

        let expected = HashMap::from([(
            "server_ip".to_string(),
            Value::new(
                Some(&"namespace".to_string()),
                ValueKind::from("value_server_ip"),
            ),
        )]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_collect_source_values() {
        #[derive(Debug, Clone)]
        struct Fake {
            option_value: Option<String>,
            string_value: String,
            u64_value: u64,
            enum_value: EnumValue,
        }

        let fake = Fake {
            option_value: Some("option value".to_string()),
            string_value: "a string value".to_string(),
            u64_value: 124,
            enum_value: EnumValue::Test,
        };

        impl Source for Fake {
            fn collect(&self) -> Result<Map<String, Value>, ConfigError> {
                let mut map = Map::new();

                let myself = self.clone();
                register_config_value_option!(map, &"namespace".to_string(), myself.option_value);
                register_config_value!(map, &"namespace".to_string(), myself.string_value);
                register_config_value!(map, &"namespace".to_string(), myself.u64_value);
                register_config_value!(map, &"namespace".to_string(), myself.enum_value);

                Ok(map)
            }

            fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
                Box::new(self.clone())
            }
        }

        let result = fake.collect().unwrap().clone();

        let expected = HashMap::from([
            (
                "option_value".to_string(),
                Value::new(
                    Some(&"namespace".to_string()),
                    ValueKind::from("option value"),
                ),
            ),
            (
                "string_value".to_string(),
                Value::new(
                    Some(&"namespace".to_string()),
                    ValueKind::from("a string value"),
                ),
            ),
            (
                "u64_value".to_string(),
                Value::new(Some(&"namespace".to_string()), ValueKind::from(124_u64)),
            ),
            (
                "enum_value".to_string(),
                Value::new(Some(&"namespace".to_string()), ValueKind::from("Test")),
            ),
        ]);

        assert_eq!(expected, result);
    }
}
