//! Commands to generate a markdown documentation for the command line.

// TODO: Some Configuration could not be generated properly because there is a lack of information.
// - We don't know which parameter is required or not.
// - In aggregator, Configuration struct contains all parameters but it's not possible to know which sub command use one parameter.

mod extract_clap_info;
mod markdown_formatter;
mod test_doc_macro;

use clap::{Command, Parser};
use std::collections::BTreeMap;
use std::fs::File;
use std::io::Write;

const DEFAULT_OUTPUT_FILE_TEMPLATE: &str = "[PROGRAM NAME]-command-line.md";

/// Information to document a field
#[derive(Clone, Default, Debug)]
pub struct FieldDoc {
    /// Name of the parameter
    pub parameter: String,
    /// Long option for the command line
    pub command_line_long: String,
    /// Short option for the command line
    pub command_line_short: String,
    /// Environment variable
    pub environment_variable: String,
    /// Description of the parameter
    pub description: String,
    /// Default value
    pub default_value: Option<String>,
    /// Usage example
    pub example: Option<String>,
    /// Is a mandatory parameter
    pub is_mandatory: bool,
}

/// Information about the struct.
#[derive(Clone, Default, Debug)]
pub struct StructDoc {
    /// List of fields
    pub data: Vec<FieldDoc>,
}

impl StructDoc {
    /// Create an empty struct.
    pub fn new() -> StructDoc {
        StructDoc { data: vec![] }
    }

    /// Add information about one parameter.
    pub fn add_param(
        &mut self,
        name: &str,
        description: &str,
        default: Option<String>,
        example: Option<String>,
    ) {
        let field_doc = FieldDoc {
            parameter: name.to_string(),
            command_line_long: "".to_string(),
            command_line_short: "".to_string(),
            environment_variable: "".to_string(),
            description: description.to_string(),
            default_value: default,
            example,
            is_mandatory: false,
        };
        self.data.push(field_doc);
    }

    /// Merge two StructDoc into a third one.
    pub fn merge_struct_doc(&self, s2: &StructDoc) -> StructDoc {
        let mut data_map1 = self
            .data
            .iter()
            .map(|field_doc| (field_doc.parameter.clone(), field_doc.clone()))
            .collect::<BTreeMap<_, _>>();

        for field_doc in s2.data.iter() {
            if !data_map1.contains_key(&field_doc.parameter) {
                data_map1.insert(field_doc.parameter.clone(), field_doc.clone());
            } else {
                let mut d = data_map1.get(&field_doc.parameter).unwrap().clone();
                if d.default_value.is_none() {
                    d.default_value = field_doc.default_value.clone();
                }
                if d.example.is_none() {
                    d.example = field_doc.example.clone();
                }
                data_map1.insert(field_doc.parameter.clone(), d);
            }
        }
        let result = StructDoc {
            data: data_map1.values().cloned().collect(),
        };
        result
    }
}

/// Extractor for struct without Default trait.
pub trait DocExtractor {
    /// Extract information used to generate documentation.
    fn extract() -> StructDoc;
}

/// Extractor for struct with Default trait.
pub trait DocExtractorDefault {
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
            format!("{}-command-line.md", cmd_name)
        } else {
            self.output.clone()
        };

        match File::create(&output) {
            Ok(mut buffer) => {
                if write!(buffer, "\n{}", doc).is_err() {
                    return Err(format!("Error writing in {}", output));
                }
                println!("Documentation generated in file `{}`", &output);
            }
            _ => return Err(format!("Could not create {}", output)),
        };
        Ok(())
    }

    /// Generate the command line documentation.
    pub fn execute(&self, cmd_to_document: &mut Command) -> Result<(), String> {
        self.execute_with_configurations(cmd_to_document, &[])
    }

    /// Generate the command line documentation with config info.
    pub fn execute_with_configurations(
        &self,
        cmd_to_document: &mut Command,
        configs_info: &[StructDoc],
    ) -> Result<(), String> {
        let mut iter_config = configs_info.iter();
        let mut merged_struct_doc = StructDoc::new();
        for next_config in &mut iter_config {
            merged_struct_doc = merged_struct_doc.merge_struct_doc(next_config);
        }

        let doc =
            markdown_formatter::doc_markdown_with_config(cmd_to_document, Some(&merged_struct_doc));
        let cmd_name = cmd_to_document.get_name();

        self.save_doc(cmd_name, format!("\n{}", doc).as_str())
    }
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use super::*;

    #[test]
    fn test_merge_struct_doc() {
        let s1 = {
            let mut s = StructDoc::default();
            s.add_param(
                "A",
                "Param first A",
                Some("default A".to_string()),
                Some("example A".to_string()),
            );
            s.add_param("B", "Param first B", None, None);
            s.add_param(
                "C",
                "Param first C",
                Some("default C".to_string()),
                Some("example C".to_string()),
            );
            s.add_param("D", "Param first D", None, None);
            s
        };

        let s2 = {
            let mut s = StructDoc::default();
            s.add_param("A", "Param second A", None, None);
            s.add_param(
                "B",
                "Param second B",
                Some("default B".to_string()),
                Some("example B".to_string()),
            );
            s.add_param("E", "Param second E", None, None);
            s.add_param(
                "F",
                "Param second F",
                Some("default F".to_string()),
                Some("example F".to_string()),
            );
            s
        };

        let result = s1.merge_struct_doc(&s2);

        let data = result.data;
        let data_map = data
            .into_iter()
            .map(|field_doc| (field_doc.parameter.clone(), field_doc))
            .collect::<HashMap<_, _>>();

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
    }

    #[test]
    fn test_merge_struct_doc_should_keep_the_order() {
        let values = ["A", "B", "C", "D", "E", "F", "G"];
        let s1 = {
            let mut s = StructDoc::default();
            for value in values.iter() {
                s.add_param(value, value, None, None);
            }
            s
        };

        let s2 = s1.clone();

        for (index, value) in values.iter().enumerate() {
            assert_eq!(value, &s1.data[index].parameter);
            assert_eq!(value, &s2.data[index].parameter);
        }

        let result = s1.merge_struct_doc(&s2);
        for (index, value) in values.iter().enumerate() {
            assert_eq!(value, &result.data[index].parameter);
        }
    }
}
