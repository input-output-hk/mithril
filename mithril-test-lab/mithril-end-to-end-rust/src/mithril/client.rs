use crate::mithril::MithrilCommand;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug)]
pub struct Client {
    command: MithrilCommand,
}

#[derive(Debug)]
pub enum ClientCommand {
    List(),
    Show { digest: String },
    Download { digest: String },
    Restore { digest: String },
}

impl Client {
    pub fn new(
        aggregator_endpoint: String,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let env = HashMap::from([
            ("NETWORK", "testnet"),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
        ]);
        let args = vec!["-vvv"];

        let command = MithrilCommand::new("mithril-client", work_dir, bin_dir, env, &args)?;
        Ok(Self { command })
    }

    pub async fn run(&mut self, command: ClientCommand) -> Result<(), String> {
        let args = match command {
            ClientCommand::List() => vec![],
            ClientCommand::Show { digest } => vec!["show".to_string(), digest],
            ClientCommand::Download { digest } => vec!["download".to_string(), digest],
            ClientCommand::Restore { digest } => vec!["restore".to_string(), digest],
        };

        let mut child = self.command.start(&args);
        match child.wait().await {
            Ok(status) => {
                if status.success() {
                    Ok(())
                } else {
                    self.command.dump_logs_to_stdout().await?;

                    Err(match status.code() {
                        Some(c) => format!("mithril-signer exited with code: {}", c),
                        None => "mithril-signer was terminated with a signal".to_string(),
                    })
                }
            }
            Err(error) => Err(error.to_string()),
        }
    }
}
