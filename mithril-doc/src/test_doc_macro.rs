#[cfg(test)]
mod tests {
    use crate::{DocExtractor, DocExtractorDefault, FieldDoc, StructDoc};
    use config::{Map, Source, Value, ValueKind};

    #[allow(dead_code)]
    #[derive(Debug, Clone, mithril_doc_derive::DocExtractor)]
    struct MyConfiguration {
        /// Execution environment
        #[example = "dev"]
        environment: String,
    }

    #[derive(Debug, Clone, mithril_doc_derive::DocExtractorDefault)]
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
            result.insert(
                "environment".to_string(),
                Value::new(Some(&namespace), ValueKind::from(myself.environment)),
            );
            Ok(result)
        }
    }

    // TODO May be part of StructDoc.
    pub fn get_field<'a>(struct_doc: &'a StructDoc, name: &str) -> &'a FieldDoc {
        let mut fields = struct_doc.data.iter().filter(|f| f.parameter == name);

        assert_eq!(1, fields.clone().count());
        fields.next().unwrap()
    }

    #[test]
    fn test_extract_struct_of_default_configuration() {
        let doc = MyDefaultConfiguration::extract();
        let field = get_field(&doc, "environment");

        assert_eq!("environment", field.parameter);
        assert_eq!("Execution environment", field.description);
        assert_eq!("prod", field.default_value.as_ref().unwrap());
    }

    #[test]
    fn test_extract_struct_of_configuration() {
        let doc = MyConfiguration::extract();
        let field = get_field(&doc, "environment");

        assert_eq!("environment", field.parameter);
        assert_eq!("Execution environment", field.description);
        assert_eq!(None, field.default_value);
    }

    #[test]
    fn test_extract_example_of_configuration() {
        {
            let doc_with_example = MyConfiguration::extract();
            let field = get_field(&doc_with_example, "environment");
            assert_eq!(Some("dev".to_string()), field.example);
        }
        {
            let doc_without_example = MyDefaultConfiguration::extract();
            let field = get_field(&doc_without_example, "environment");
            assert_eq!(None, field.example);
        }
    }
}
