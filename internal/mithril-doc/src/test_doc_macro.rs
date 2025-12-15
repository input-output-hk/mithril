#[cfg(test)]
mod tests {
    use config::{Map, Source, Value};
    use mithril_cli_helper::register_config_value;

    use crate::{Documenter, DocumenterDefault, StructDoc};

    #[allow(dead_code)]
    #[derive(Debug, Clone, mithril_doc_derive::Documenter)]
    struct MyConfiguration {
        /// Optional parameter
        version: Option<String>,

        /// Execution environment
        #[example = "dev"]
        environment: String,
    }

    #[derive(Debug, Clone, mithril_doc_derive::DocumenterDefault)]
    struct MyDefaultConfiguration {
        /// Execution environment
        environment: String,
    }
    impl Default for MyDefaultConfiguration {
        fn default() -> Self {
            Self {
                environment: "prod".to_string(),
            }
        }
    }

    impl Source for MyDefaultConfiguration {
        fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
            Box::new(self.clone())
        }

        fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
            let mut result = Map::new();
            let namespace = "default configuration".to_string();

            let myself = self.clone();
            register_config_value!(result, &namespace, myself.environment);

            Ok(result)
        }
    }

    #[test]
    fn test_extract_struct_of_default_configuration() {
        let doc = MyDefaultConfiguration::extract();
        let field = doc.get_field("environment").unwrap();

        assert_eq!("environment", field.parameter);
        assert_eq!("ENVIRONMENT", field.environment_variable.as_ref().unwrap());
        assert_eq!("Execution environment", field.description);
        assert_eq!("prod", field.default_value.as_ref().unwrap());
    }

    #[test]
    fn test_extract_struct_of_configuration() {
        let doc = MyConfiguration::extract();
        let field = doc.get_field("environment").unwrap();

        assert_eq!("environment", field.parameter);
        assert_eq!("ENVIRONMENT", field.environment_variable.as_ref().unwrap());
        assert_eq!("Execution environment", field.description);
        assert_eq!(None, field.default_value);
    }

    #[test]
    fn test_extract_example_of_configuration() {
        {
            let doc_with_example = MyConfiguration::extract();
            let field = doc_with_example.get_field("environment").unwrap();
            assert_eq!(Some("dev".to_string()), field.example);
        }
        {
            let doc_without_example = MyDefaultConfiguration::extract();
            let field = doc_without_example.get_field("environment").unwrap();
            assert_eq!(None, field.example);
        }
    }

    #[test]
    fn test_extract_configuration_optional_field() {
        let doc_with_example = MyConfiguration::extract();
        assert!(!doc_with_example.get_field("version").unwrap().is_mandatory);
        assert!(doc_with_example.get_field("environment").unwrap().is_mandatory);
    }

    #[test]
    fn test_extract_configuration_fields_in_declaration_order() {
        let doc_with_example = MyConfiguration::extract();

        let fields = doc_with_example.get_ordered_data();

        assert_eq!(fields[0].parameter, "version");
        assert_eq!(fields[1].parameter, "environment");
    }
}
