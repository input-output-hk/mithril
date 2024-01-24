//! Commands to generate a markdown documentation for the command line.


mod extract_clap_info;
mod markdown_formatter;
mod test_doc_macro;

use std::{collections::HashMap, fs::File};
use std::io::Write;
use clap::{Parser, Command};

use crate::StdResult;

const DEFAULT_OUTPUT_FILE_TEMPLATE: &str = "[PROGRAM NAME]-command-line.md";

/// Information to document a field
#[derive(Clone, Default)]
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
    pub example: String,
    /// Is a mandatory parameter
    pub is_mandatory: bool,
}

/// Information about the struct.
#[derive(Clone, Default)]
pub struct StructDoc {
    /// List of fields
    pub data: Vec<FieldDoc>,
}

impl StructDoc {
    /// Create an empty struct.
    pub fn new() -> StructDoc {
        StructDoc {
            data: vec!(),
        }
    }

    /// Add information about one parameter.
    pub fn add_param(&mut self, name: &str, description: &str, default: Option<String>) {
        let field_doc = FieldDoc{
            parameter: name.to_string(),
            command_line_long: "".to_string(),
            command_line_short: "".to_string(),
            environment_variable: "".to_string(),
            description: description.to_string(),
            default_value: default,
            example: "".to_string(),
            is_mandatory: false,
        };
        self.data.push(field_doc);
    }

    /// Merge two StructDoc into a third one.
    pub fn merge_struct_doc(&self, s2: &StructDoc) -> StructDoc {
    
        let mut data_map1 = self.data.iter()
            .map(|field_doc| (field_doc.parameter.clone(), field_doc.clone()))
            .collect::<HashMap<_,_>>();

        for field_doc in s2.data.iter() {
            if !data_map1.contains_key(&field_doc.parameter) {
                data_map1.insert(field_doc.parameter.clone(), field_doc.clone());
            } else {
            
                let mut d = data_map1.get(&field_doc.parameter).unwrap().clone();
                if d.default_value.is_none() {
                    d.default_value = field_doc.default_value.clone();
                    data_map1.insert(field_doc.parameter.clone(), d);
                }
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
    fn save_doc(&self, cmd_name: &str, doc: &str) -> StdResult<()> {
        let output = if self.output.as_str() == DEFAULT_OUTPUT_FILE_TEMPLATE {
            format!("{}-command-line.md", cmd_name)
        } else {
            self.output.clone()
        };
        let mut buffer: File = File::create(&output)?;
        write!(buffer, "\n{}", doc)?;
        println!("Documentation generated in file `{}`", &output);
        Ok(())
    }

    /// Generate the command line documentation.
    pub fn execute(&self, cmd_to_document: &mut Command) -> StdResult<()> {
        self.execute_with_configurations(cmd_to_document, &vec!())
    }

    /// Generate the command line documentation with config info.
    pub fn execute_with_configurations(&self, cmd_to_document: &mut Command, configs_info: &Vec<StructDoc>) -> StdResult<()> { 
    
        let mut iter_config = configs_info.iter();
        let mut merged_struct_doc = StructDoc::new();
        while let Some(next_config) = iter_config.next() {
            merged_struct_doc = merged_struct_doc.merge_struct_doc(&next_config);
        };

        let doc = markdown_formatter::doc_markdown_with_config(cmd_to_document, Some(&merged_struct_doc));
        let cmd_name = cmd_to_document.get_name();
 
        self.save_doc(cmd_name, format!("\n{}", doc).as_str())
    }
}


#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_merge_struct_doc() {
        let s1 = {
            let mut s = StructDoc::default();
            s.add_param("A", "Param first A", Some("default A".to_string()));
            s.add_param("B", "Param first B", None);
            s.add_param("C", "Param first C", Some("default C".to_string()));
            s.add_param("D", "Param first D", None);
            s
        };

        let s2 = {
            let mut s = StructDoc::default();
            s.add_param("A", "Param second A", None);
            s.add_param("B", "Param second B", Some("default B".to_string()));
            s.add_param("E", "Param second E", None);
            s.add_param("F", "Param second F", Some("default F".to_string()));
            s
        };

        let result = s1.merge_struct_doc(&s2);

        let data = result.data;
        let data_map = data.into_iter().map(|field_doc| (field_doc.parameter.clone(), field_doc)).collect::<HashMap<_,_>>();

        assert_eq!(6, data_map.iter().count());
        assert_eq!("Param first A", data_map.get("A").unwrap().description);
        assert_eq!("Param first B", data_map.get("B").unwrap().description);
        assert_eq!("Param first C", data_map.get("C").unwrap().description);
        assert_eq!("Param first D", data_map.get("D").unwrap().description);
        assert_eq!("Param second E", data_map.get("E").unwrap().description);
        assert_eq!("Param second F", data_map.get("F").unwrap().description);

        assert_eq!(Some("default A".to_string()), data_map.get("A").unwrap().default_value);
        assert_eq!(Some("default B".to_string()), data_map.get("B").unwrap().default_value);
        assert_eq!(Some("default C".to_string()), data_map.get("C").unwrap().default_value);
        assert_eq!(None, data_map.get("D").unwrap().default_value);
        assert_eq!(None, data_map.get("E").unwrap().default_value);
        assert_eq!(Some("default F".to_string()), data_map.get("F").unwrap().default_value);

    }
}