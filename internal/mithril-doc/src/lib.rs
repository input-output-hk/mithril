//! Commands to generate a markdown documentation for the command line.

// LIMITATIONS: Some `Configuration` cannot be generated as precisely as we would like because there is a lack of information in the code.
// - We don't know which parameter is required or not.
// - In aggregator, Configuration struct contains all parameters but it's not possible to know which sub command use one parameter.

mod extract_clap_info;
mod markdown_formatter;
mod test_doc_macro;

use clap::{Command, Parser};
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::Write;

pub use mithril_doc_derive::{self, *};

const DEFAULT_OUTPUT_FILE_TEMPLATE: &str = "[PROGRAM NAME]-command-line.md";

/// Information to document a field
#[derive(Clone, Default, Debug, PartialEq)]
pub struct FieldDoc {
    /// Name of the parameter
    pub parameter: String,
    /// Long option for the command line
    pub command_line_long: String,
    /// Short option for the command line
    pub command_line_short: String,
    /// Environment variable
    pub environment_variable: Option<String>,
    /// Description of the parameter
    pub description: String,
    /// Default value
    pub default_value: Option<String>,
    /// Usage example
    pub example: Option<String>,
    /// Is a mandatory parameter
    pub is_mandatory: bool,
}

impl FieldDoc {
    fn merge_field(&mut self, field_doc: &FieldDoc) {
        if self.default_value.is_none() {
            self.default_value.clone_from(&field_doc.default_value);
        }
        if self.example.is_none() {
            self.example.clone_from(&field_doc.example);
        }
        if self.environment_variable.is_none() {
            self.environment_variable.clone_from(&field_doc.environment_variable);
        }
    }
}

/// Information about the struct.
#[derive(Clone, Default, Debug)]
pub struct StructDoc {
    /// Parameter names in the insertion order.
    parameter_order: Vec<String>,

    /// Parameters description
    parameters: BTreeMap<String, FieldDoc>,
}

impl StructDoc {
    /// Create an empty struct.
    pub fn new(fields: Vec<FieldDoc>) -> StructDoc {
        let mut struct_doc = StructDoc {
            parameter_order: vec![],
            parameters: BTreeMap::new(),
        };
        for field in fields {
            struct_doc.add_field(field);
        }
        struct_doc
    }

    /// Add information about one parameter.
    pub fn add_param(
        &mut self,
        name: &str,
        description: &str,
        environment_variable: Option<String>,
        default: Option<String>,
        example: Option<String>,
        is_mandatory: bool,
    ) {
        let field_doc = FieldDoc {
            parameter: name.to_string(),
            command_line_long: "".to_string(),
            command_line_short: "".to_string(),
            environment_variable,
            description: description.to_string(),
            default_value: default,
            example,
            is_mandatory,
        };
        self.parameter_order.push(field_doc.parameter.to_string());
        self.parameters.insert(field_doc.parameter.to_string(), field_doc);
    }

    fn add_field(&mut self, field_doc: FieldDoc) {
        self.parameter_order.push(field_doc.parameter.to_string());
        self.parameters.insert(field_doc.parameter.to_string(), field_doc);
    }

    /// Merge two StructDoc into a third one.
    pub fn merge_struct_doc(&self, s2: &StructDoc) -> StructDoc {
        let mut struct_doc_merged =
            StructDoc::new(self.get_ordered_data().into_iter().cloned().collect());
        for field_doc in s2.get_ordered_data().into_iter() {
            let key = field_doc.parameter.clone();
            if let Some(parameter) = struct_doc_merged.parameters.get_mut(&key) {
                parameter.merge_field(field_doc);
            } else {
                struct_doc_merged.add_field(field_doc.clone());
            }
        }
        struct_doc_merged
    }

    /// Get a field by its name.
    pub fn get_field(&self, name: &str) -> Option<&FieldDoc> {
        self.parameters.get(name)
    }

    pub fn get_ordered_data(&self) -> Vec<&FieldDoc> {
        self.parameter_order
            .iter()
            .map(|parameter| self.parameters.get(parameter).unwrap())
            .collect()
    }
}

/// Extractor for struct without Default trait.
pub trait Documenter {
    /// Extract information used to generate documentation.
    fn extract() -> StructDoc;
}

/// Extractor for struct with Default trait.
pub trait DocumenterDefault {
    /// Extract information used to generate documentation.
    fn extract() -> StructDoc;
}

/// Generate documentation
#[derive(Parser, Debug, PartialEq, Clone)]
pub struct GenerateDocCommands {
    /// Generated documentation file
    #[clap(long, default_value = DEFAULT_OUTPUT_FILE_TEMPLATE)]
    output: String,
}

impl GenerateDocCommands {
    fn save_doc(&self, cmd_name: &str, doc: &str) -> Result<(), String> {
        let output = if self.output.as_str() == DEFAULT_OUTPUT_FILE_TEMPLATE {
            format!("{cmd_name}-command-line.md")
        } else {
            self.output.clone()
        };

        match File::create(&output) {
            Ok(mut buffer) => {
                if write!(buffer, "\n{doc}").is_err() {
                    return Err(format!("Error writing in {output}"));
                }
                println!("Documentation generated in file `{}`", &output);
            }
            _ => return Err(format!("Could not create {output}")),
        };
        Ok(())
    }

    /// Generate the command line documentation.
    pub fn execute(&self, cmd_to_document: &mut Command) -> Result<(), String> {
        self.execute_with_configurations(cmd_to_document, HashMap::new())
    }

    /// Generate the command line documentation with config info.
    pub fn execute_with_configurations(
        &self,
        cmd_to_document: &mut Command,
        configs_info: HashMap<String, StructDoc>,
    ) -> Result<(), String> {
        let doc = markdown_formatter::doc_markdown_with_config(cmd_to_document, configs_info);
        let cmd_name = cmd_to_document.get_name();

        println!(
            "Please note: the documentation generated is not able to indicate the environment variables used by the commands."
        );
        self.save_doc(cmd_name, format!("\n{doc}").as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_field_must_return_first_field_by_parameter_name() {
        let mut struct_doc = StructDoc::default();
        struct_doc.add_param("A", "Param first A", None, None, None, true);
        struct_doc.add_param("B", "Param first B", None, None, None, true);

        let retrieved_field = struct_doc.get_field("A").unwrap();

        assert_eq!(retrieved_field.description, "Param first A");
    }

    #[test]
    fn test_get_field_must_return_none_if_parameter_does_not_exist() {
        let mut struct_doc = StructDoc::default();
        struct_doc.add_param("A", "Param first A", None, None, None, true);
        struct_doc.add_param("B", "Param first B", None, None, None, true);

        let retrieved_field = struct_doc.get_field("X");

        assert_eq!(retrieved_field, None);
    }

    #[test]
    fn test_merge_struct_doc() {
        let s1 = {
            let mut s = StructDoc::default();
            s.add_param(
                "A",
                "Param first A",
                Some("env A".to_string()),
                Some("default A".to_string()),
                Some("example A".to_string()),
                true,
            );
            s.add_param("B", "Param first B", None, None, None, true);
            s.add_param(
                "C",
                "Param first C",
                Some("env C".to_string()),
                Some("default C".to_string()),
                Some("example C".to_string()),
                true,
            );
            s.add_param("D", "Param first D", None, None, None, true);
            s
        };

        let s2 = {
            let mut s = StructDoc::default();
            s.add_param("A", "Param second A", None, None, None, true);
            s.add_param(
                "B",
                "Param second B",
                Some("env B".to_string()),
                Some("default B".to_string()),
                Some("example B".to_string()),
                true,
            );
            s.add_param("E", "Param second E", None, None, None, true);
            s.add_param(
                "F",
                "Param second F",
                Some("env F".to_string()),
                Some("default F".to_string()),
                Some("example F".to_string()),
                true,
            );
            s
        };

        let result = s1.merge_struct_doc(&s2);
        let data_map = result.parameters;

        assert_eq!(6, data_map.len());
        assert_eq!("Param first A", data_map.get("A").unwrap().description);
        assert_eq!("Param first B", data_map.get("B").unwrap().description);
        assert_eq!("Param first C", data_map.get("C").unwrap().description);
        assert_eq!("Param first D", data_map.get("D").unwrap().description);
        assert_eq!("Param second E", data_map.get("E").unwrap().description);
        assert_eq!("Param second F", data_map.get("F").unwrap().description);

        assert_eq!(
            Some("default A".to_string()),
            data_map.get("A").unwrap().default_value
        );
        assert_eq!(
            Some("default B".to_string()),
            data_map.get("B").unwrap().default_value
        );
        assert_eq!(
            Some("default C".to_string()),
            data_map.get("C").unwrap().default_value
        );
        assert_eq!(None, data_map.get("D").unwrap().default_value);
        assert_eq!(None, data_map.get("E").unwrap().default_value);
        assert_eq!(
            Some("default F".to_string()),
            data_map.get("F").unwrap().default_value
        );

        assert_eq!(
            Some("example A".to_string()),
            data_map.get("A").unwrap().example
        );
        assert_eq!(
            Some("example B".to_string()),
            data_map.get("B").unwrap().example
        );
        assert_eq!(
            Some("example C".to_string()),
            data_map.get("C").unwrap().example
        );
        assert_eq!(None, data_map.get("D").unwrap().example);
        assert_eq!(None, data_map.get("E").unwrap().example);
        assert_eq!(
            Some("example F".to_string()),
            data_map.get("F").unwrap().example
        );

        assert_eq!(
            Some("env A".to_string()),
            data_map.get("A").unwrap().environment_variable
        );
        assert_eq!(
            Some("env B".to_string()),
            data_map.get("B").unwrap().environment_variable
        );
        assert_eq!(
            Some("env C".to_string()),
            data_map.get("C").unwrap().environment_variable
        );
        assert_eq!(None, data_map.get("D").unwrap().environment_variable);
        assert_eq!(None, data_map.get("E").unwrap().environment_variable);
        assert_eq!(
            Some("env F".to_string()),
            data_map.get("F").unwrap().environment_variable
        );
    }

    #[test]
    fn test_merge_struct_doc_should_keep_the_order() {
        fn build_struct_doc(values: &[&str]) -> StructDoc {
            let mut struct_doc = StructDoc::default();
            for value in values.iter() {
                struct_doc.add_param(value, value, None, None, None, true);
            }

            assert_eq!(
                struct_doc
                    .get_ordered_data()
                    .iter()
                    .map(|data| data.parameter.to_string())
                    .collect::<Vec<_>>(),
                values
            );
            struct_doc
        }

        let values_1 = ["A", "E", "C", "B", "D"];
        let s1 = build_struct_doc(&values_1);

        let values_2 = ["G", "D", "E", "F", "C"];
        let s2 = build_struct_doc(&values_2);

        let result = s1.merge_struct_doc(&s2);
        assert_eq!(
            result
                .get_ordered_data()
                .iter()
                .map(|data| data.parameter.to_string())
                .collect::<Vec<_>>(),
            ["A", "E", "C", "B", "D", "G", "F"]
        );
    }
}
