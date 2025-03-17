//! Utilities to register config parameters.

/// Register a parameter in the config map.
#[macro_export]
macro_rules! register {
    ( $map:ident, $namespace:expr, $command: ident, $value:expr ) => {{
        $map.insert(
            stringify!($command).to_string(),
            config::Value::new(Some($namespace), $value),
        );
    }};
}

/// Register a optional parameter in the config map.
#[macro_export]
macro_rules! register_parameter_opt {
    ( $map:ident, $namespace:expr, $self:ident.$command:ident ) => {{
        if let Some(value) = $self.$command.clone() {
            register!($map, $namespace, $command, value);
        }
    }};
    ( $map:ident, $namespace:expr, $self:ident.$command:ident, $mapping:expr ) => {{
        if let Some(value) = $self.$command.clone() {
            register!($map, $namespace, $command, $mapping(value));
        }
    }};
}

/// Register a boolean parameter in the config map.
#[macro_export]
macro_rules! register_parameter_bool {
    ( $map:ident, $namespace:expr, $self:ident.$command:ident ) => {{
        if $self.$command {
            register!($map, $namespace, $command, true);
        }
    }};
}

/// Register a parameter in the config map.
#[macro_export]
macro_rules! register_parameter {
    ( $map:ident, $namespace:expr, $self:ident.$command:ident ) => {{
        register!($map, $namespace, $command, $self.$command);
    }};
}

#[cfg(test)]
mod tests {
    use config::{ConfigError, Map, Source, Value, ValueKind};
    use std::collections::HashMap;

    #[test]
    fn test_register_macro() {
        let mut map = HashMap::new();
        register!(
            map,
            &"namespace".to_string(),
            server_ip,
            Some("value_server_ip".to_string())
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

    #[test]
    fn test_register_parameter_macro_add_value() {
        struct Fake {
            string_value: String,
            u64_value: u64,
        }

        let fake = Fake {
            string_value: "a string value".to_string(),
            u64_value: 124,
        };

        let mut map = HashMap::new();
        register_parameter!(map, &"namespace".to_string(), fake.string_value);
        register_parameter!(map, &"namespace".to_string(), fake.u64_value);

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
                Value::new(Some(&"namespace".to_string()), ValueKind::from(124 as u64)),
            ),
        ]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_parameter_option_macro_not_add_none_value() {
        struct Fake {
            option_with_value: Option<String>,
            option_none: Option<String>,
        }

        let fake = Fake {
            option_with_value: Some("value_of_option".to_string()),
            option_none: None,
        };

        let mut map = HashMap::new();
        register_parameter_opt!(map, &"namespace".to_string(), fake.option_with_value);
        register_parameter_opt!(map, &"namespace".to_string(), fake.option_none);

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
    fn test_register_parameter_option_macro_with_mapping_transform_value_before_adding_it() {
        struct Fake {
            option_with_value: Option<String>,
            option_none: Option<String>,
        }

        let fake = Fake {
            option_with_value: Some("value_of_option".to_string()),
            option_none: None,
        };

        let mut map = HashMap::new();
        register_parameter_opt!(
            map,
            &"namespace".to_string(),
            fake.option_with_value,
            |v: String| format!("mapped_value from {}", v)
        );
        register_parameter_opt!(map, &"namespace".to_string(), fake.option_none);

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
    fn test_register_parameter_bool_macro_add_only_true_values() {
        struct Fake {
            bool_true: bool,
            bool_false: bool,
        }

        let fake = Fake {
            bool_true: true,
            bool_false: false,
        };

        let mut map = HashMap::new();
        register_parameter_bool!(map, &"namespace".to_string(), fake.bool_true);
        register_parameter_bool!(map, &"namespace".to_string(), fake.bool_false);

        let expected = HashMap::from([(
            "bool_true".to_string(),
            Value::new(Some(&"namespace".to_string()), ValueKind::from(true)),
        )]);

        assert_eq!(expected, map);
    }

    #[test]
    fn test_register_parameter_macro() {
        struct Fake {
            server_ip: Option<String>,
        }

        let fake = Fake {
            server_ip: Some("value_server_ip".to_string()),
        };

        let mut map = HashMap::new();
        register_parameter!(map, &"namespace".to_string(), fake.server_ip);

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
            server_ip: Option<String>,
        }

        let fake = Fake {
            server_ip: Some("value_server_ip".to_string()),
        };

        impl Source for Fake {
            fn collect(&self) -> Result<Map<String, Value>, ConfigError> {
                let mut map = Map::new();
                register_parameter_opt!(map, &"namespace".to_string(), self.server_ip);
                Ok(map)
            }

            fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
                Box::new(self.clone())
            }
        }

        let result = fake.collect().unwrap().clone();

        let expected = HashMap::from([(
            "server_ip".to_string(),
            Value::new(
                Some(&"namespace".to_string()),
                ValueKind::from("value_server_ip"),
            ),
        )]);

        assert_eq!(expected, result);
    }
}
