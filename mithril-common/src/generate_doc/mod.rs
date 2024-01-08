//! Commands to generate a markdown documentation for the command line.

mod markdown_formatter;

use std::fs::File;
use std::io::Write;
use clap::{Parser, Command};

use crate::StdResult;

const DEFAULT_OUTPUT_FILE_TEMPLATE: &str = "[PROGRAM NAME]-command-line.md";

/// Generate documentation
#[derive(Parser, Debug, Clone)]
pub struct GenerateDocCommands {
    /// Generated documentation file 
    #[clap(long, default_value = DEFAULT_OUTPUT_FILE_TEMPLATE)]
    output: String,
}
impl GenerateDocCommands {
    /// Generate the command line documentation.
    pub async fn execute(&self, cmd_to_document: &mut Command) -> StdResult<()> {
        let cmd_name = cmd_to_document.get_name().to_owned();
        let doc = markdown_formatter::doc_markdown(cmd_to_document);
        
        let output = if self.output.as_str() == DEFAULT_OUTPUT_FILE_TEMPLATE {
            format!("{}-command-line.md", cmd_name)
        } else {
            self.output.clone()
        };
        let mut buffer: File = File::create(&output)?;
        buffer.write(b"Generated doc\n\n")?;
        buffer.write(doc.as_bytes())?;
        println!("Documentation generated in file `{}`", &output);
        Ok(())
    }
}