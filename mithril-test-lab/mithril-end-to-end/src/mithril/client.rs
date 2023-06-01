use crate::utils::MithrilCommand;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug)]
pub struct Client {
    command: MithrilCommand,
}

#[derive(Debug)]
pub enum SnapshotCommand {
    List(),
    Show { digest: String },
    Download { digest: String },
}

#[derive(Debug)]
pub struct MithrilStakeDistributionCommand;

#[derive(Debug)]
pub enum ClientCommand {
    Snapshot(SnapshotCommand),
    MithrilStakeDistribution(MithrilStakeDistributionCommand),
}

impl Client {
    pub fn new(
        aggregator_endpoint: String,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let env = HashMap::from([
            ("NETWORK", "devnet"),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
            ("GENESIS_VERIFICATION_KEY", "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d"),
        ]);
        let args = vec!["-vvv"];
        let command = MithrilCommand::new("mithril-client", work_dir, bin_dir, env, &args)?;

        Ok(Self { command })
    }

    pub async fn run(&mut self, command: ClientCommand) -> Result<(), String> {
        let args = match command {
            ClientCommand::Snapshot(subcommand) => match subcommand {
                SnapshotCommand::List() => vec!["snapshot".to_string(), "list".to_string()],
                SnapshotCommand::Show { digest } => {
                    vec!["snapshot".to_string(), "show".to_string(), digest]
                }
                SnapshotCommand::Download { digest } => {
                    vec!["snapshot".to_string(), "download".to_string(), digest]
                }
            },
            ClientCommand::MithrilStakeDistribution(_command) => Vec::new(),
        };
        let mut child = self.command.start(&args)?;

        match child.wait().await {
            Ok(status) => {
                if status.success() {
                    Ok(())
                } else {
                    self.command
                        .tail_logs(Some(format!("mithril-client {args:?}").as_str()), 20)
                        .await?;

                    Err(match status.code() {
                        Some(c) => format!("mithril-client exited with code: {c}"),
                        None => "mithril-client was terminated with a signal".to_string(),
                    })
                }
            }
            Err(error) => Err(error.to_string()),
        }
    }
}
