use crate::mithril::MithrilCommand;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct Signer {
    command: MithrilCommand,
    process: Option<Child>,
}

impl Signer {
    pub fn new(
        aggregator_endpoint: String,
        db_directory: &Path,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let env = HashMap::from([
            ("NETWORK", "testnet"),
            ("PARTY_ID", "0"),
            ("RUN_INTERVAL", "2000"),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
            ("DB_DIRECTORY", db_directory.to_str().unwrap()),
            ("STAKE_STORE_DIRECTORY", "./store/signer/stakes"),
        ]);
        let args = vec!["-vvv"];

        let command = MithrilCommand::new("mithril-signer", work_dir, bin_dir, env, &args)?;

        Ok(Self {
            command,
            process: None,
        })
    }

    pub fn start(&mut self) {
        self.process = Some(self.command.start(&[]));
    }

    pub async fn dump_logs(&self) -> Result<(), String> {
        self.command.dump_logs_to_stdout().await
    }

    pub async fn dump_logs_if_crashed(&mut self) -> Result<(), String> {
        match self.process.as_mut() {
            Some(child) => match child.try_wait() {
                Ok(Some(status)) => {
                    if !status.success() {
                        self.dump_logs().await?;
                    }
                    Ok(())
                }
                Ok(None) => Ok(()),
                Err(e) => Err(format!("failed get mithril-aggregator status: {}", e)),
            },
            None => Ok(()),
        }
    }
}
