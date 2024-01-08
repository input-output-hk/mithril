
use std::fs::File;
use std::io::Write;

use clap::{Parser, Command};

use crate::utils;
use mithril_client::MithrilResult;

/// Generate documentation
#[derive(Parser, Debug, Clone)]
pub struct GenerateDocCommands {
    /// Generated documentation file 
    #[clap(long, default_value = "generated_doc.md")]
    output: String,
}
impl GenerateDocCommands {
    pub async fn execute(&self, cmd_to_document: &mut Command) -> MithrilResult<()> {
        let doc = utils::doc_markdown(cmd_to_document);
        
        let mut buffer: File = File::create(&self.output)?;
        buffer.write(b"Generated doc\n\n")?;
        buffer.write(doc.as_bytes())?;
        println!("Documentation generated in file `{}`", &self.output);
        Ok(())
    }
}