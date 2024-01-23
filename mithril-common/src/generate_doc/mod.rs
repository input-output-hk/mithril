//! Commands to generate a markdown documentation for the command line.

mod extract_clap_info;
mod markdown_formatter;
mod test_doc_macro;

use std::fs::File;
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
    /// Generate the command line documentation.
    pub fn execute(&self, cmd_to_document: &mut Command) -> StdResult<()> {
        let cmd_name = cmd_to_document.get_name().to_owned();
        let doc = markdown_formatter::doc_markdown(cmd_to_document);
        
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

    /// Generate the command line documentation with config info.
    pub fn execute_with_configurations(&self, cmd_to_document: &mut Command, configs_info: &Vec<StructDoc>) -> StdResult<()> { 
    
        let mut iter_config = configs_info.iter();
        let mut merged_struct_doc = StructDoc::new();
        while let Some(next_config) = iter_config.next() {
            merged_struct_doc = extract_clap_info::merge_struct_doc(&merged_struct_doc, &next_config);
        };

        merged_struct_doc = extract_clap_info::merge_struct_doc(&merged_struct_doc, &extract_clap_info::extract_parameters(cmd_to_document));
        
        let config_doc =  markdown_formatter::doc_config_to_markdown(&merged_struct_doc);

        let cmd_name = cmd_to_document.get_name().to_owned();
        let doc = markdown_formatter::doc_markdown(cmd_to_document);
        
        let output = if self.output.as_str() == DEFAULT_OUTPUT_FILE_TEMPLATE {
            format!("{}-command-line.md", cmd_name)
        } else {
            self.output.clone()
        };
        let mut buffer: File = File::create(&output)?;
        write!(buffer, "\n{}\n{}", doc, config_doc)?;
        println!("Documentation generated in file `{}`", &output);
        Ok(())
    }
}