use crate::utils::MithrilCommand;
use anyhow::{anyhow, Context};
use mithril_common::{entities::TransactionHash, StdResult};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct Client {
    command: MithrilCommand,
}

#[derive(Debug)]
pub enum CardanoDbCommand {
    List(),
    Show { digest: String },
    Download { digest: String },
}

impl CardanoDbCommand {
    fn name(&self) -> String {
        match self {
            CardanoDbCommand::List() => "list".to_string(),
            CardanoDbCommand::Show { digest } => format!("show-{digest}"),
            CardanoDbCommand::Download { digest } => format!("download-{digest}"),
        }
    }

    fn cli_arg(&self) -> Vec<String> {
        match self {
            CardanoDbCommand::List() => {
                vec!["snapshot".to_string(), "list".to_string()]
            }
            CardanoDbCommand::Show { digest } => {
                vec!["snapshot".to_string(), "show".to_string(), digest.clone()]
            }
            CardanoDbCommand::Download { digest } => {
                vec!["download".to_string(), digest.clone()]
            }
        }
    }
}

#[derive(Debug)]
pub enum CardanoDbV2Command {
    List,
    Show { hash: String },
    Download { hash: String },
}

impl CardanoDbV2Command {
    fn name(&self) -> String {
        match self {
            CardanoDbV2Command::List => "list".to_string(),
            CardanoDbV2Command::Show { hash } => format!("show-{hash}"),
            CardanoDbV2Command::Download { hash } => format!("download-{hash}"),
        }
    }

    fn cli_arg(&self) -> Vec<String> {
        match self {
            CardanoDbV2Command::List => {
                vec!["snapshot".to_string(), "list".to_string()]
            }
            CardanoDbV2Command::Show { hash } => {
                vec!["snapshot".to_string(), "show".to_string(), hash.clone()]
            }
            CardanoDbV2Command::Download { hash } => {
                vec!["download".to_string(), hash.clone()]
            }
        }
    }
}

#[derive(Debug)]
pub enum MithrilStakeDistributionCommand {
    List,
    Download { hash: String },
}

impl MithrilStakeDistributionCommand {
    fn name(&self) -> String {
        match self {
            MithrilStakeDistributionCommand::List => "list".to_string(),
            MithrilStakeDistributionCommand::Download { hash } => format!("download-{hash}"),
        }
    }

    fn cli_arg(&self) -> Vec<String> {
        match self {
            MithrilStakeDistributionCommand::List => {
                vec!["list".to_string()]
            }
            MithrilStakeDistributionCommand::Download { hash } => {
                vec!["download".to_string(), hash.clone()]
            }
        }
    }
}

#[derive(Debug)]
pub enum CardanoTransactionCommand {
    ListSnapshot,
    ShowSnapshot { hash: String },
    Certify { tx_hashes: Vec<TransactionHash> },
}

impl CardanoTransactionCommand {
    fn name(&self) -> String {
        match self {
            CardanoTransactionCommand::ListSnapshot => "list-snapshot".to_string(),
            CardanoTransactionCommand::ShowSnapshot { hash } => format!("show-snapshot-{hash}"),
            CardanoTransactionCommand::Certify { tx_hashes } if tx_hashes.len() > 1 => {
                // Only output first & last hash to avoid too long filenames
                format!("certify-{}..{}", tx_hashes[0], tx_hashes.last().unwrap())
            }
            CardanoTransactionCommand::Certify { tx_hashes } => {
                format!("certify-{}", tx_hashes.first().unwrap_or(&String::new()))
            }
        }
    }

    fn cli_arg(&self) -> Vec<String> {
        match self {
            CardanoTransactionCommand::ListSnapshot => {
                vec!["snapshot".to_string(), "list".to_string()]
            }
            CardanoTransactionCommand::ShowSnapshot { hash } => {
                vec!["snapshot".to_string(), "show".to_string(), hash.clone()]
            }
            CardanoTransactionCommand::Certify { tx_hashes } => {
                vec!["certify".to_string(), tx_hashes.join(",")]
            }
        }
    }
}

#[derive(Debug)]
pub enum CardanoStakeDistributionCommand {
    List,
    Download { unique_identifier: String },
}

impl CardanoStakeDistributionCommand {
    fn name(&self) -> String {
        match self {
            CardanoStakeDistributionCommand::List => "list".to_string(),
            CardanoStakeDistributionCommand::Download { unique_identifier } => {
                format!("download-{unique_identifier}")
            }
        }
    }

    fn cli_arg(&self) -> Vec<String> {
        match self {
            CardanoStakeDistributionCommand::List => {
                vec!["list".to_string()]
            }
            CardanoStakeDistributionCommand::Download { unique_identifier } => {
                vec!["download".to_string(), unique_identifier.clone()]
            }
        }
    }
}

#[derive(Debug)]
pub enum ClientCommand {
    CardanoDb(CardanoDbCommand),
    MithrilStakeDistribution(MithrilStakeDistributionCommand),
    CardanoTransaction(CardanoTransactionCommand),
    CardanoStakeDistribution(CardanoStakeDistributionCommand),
    CardanoDbV2(CardanoDbV2Command),
}

impl ClientCommand {
    fn name(&self) -> String {
        match self {
            ClientCommand::CardanoDb(cmd) => format!("cardano-db-{}", cmd.name()),
            ClientCommand::MithrilStakeDistribution(cmd) => {
                format!("msd-{}", cmd.name())
            }
            ClientCommand::CardanoTransaction(cmd) => {
                format!("ctx-{}", cmd.name())
            }
            ClientCommand::CardanoStakeDistribution(cmd) => {
                format!("csd-{}", cmd.name())
            }
            ClientCommand::CardanoDbV2(cmd) => {
                format!("cdbv2-{}", cmd.name())
            }
        }
    }

    fn cli_arg(&self) -> Vec<String> {
        let mut args = match self {
            ClientCommand::CardanoDb(cmd) => {
                [vec!["cardano-db".to_string()], cmd.cli_arg()].concat()
            }
            ClientCommand::MithrilStakeDistribution(cmd) => [
                vec!["mithril-stake-distribution".to_string()],
                cmd.cli_arg(),
            ]
            .concat(),
            ClientCommand::CardanoTransaction(cmd) => {
                [vec!["cardano-transaction".to_string()], cmd.cli_arg()].concat()
            }
            ClientCommand::CardanoStakeDistribution(cmd) => [
                vec!["cardano-stake-distribution".to_string()],
                cmd.cli_arg(),
            ]
            .concat(),
            ClientCommand::CardanoDbV2(cmd) => [
                vec!["--unstable".to_string(), "cardano-db-v2".to_string()],
                cmd.cli_arg(),
            ]
            .concat(),
        };
        args.push("--json".to_string());

        args
    }
}

impl Client {
    pub fn new(aggregator_endpoint: String, work_dir: &Path, bin_dir: &Path) -> StdResult<Self> {
        let env = HashMap::from([
            ("GENESIS_VERIFICATION_KEY", "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d"),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
        ]);
        let args = vec!["-vvv"];
        let command = MithrilCommand::new("mithril-client", work_dir, bin_dir, env, &args)?;

        Ok(Self { command })
    }

    pub async fn run(&mut self, command: ClientCommand) -> StdResult<PathBuf> {
        let output_path = self
            .command
            .set_output_filename(&format!("mithril-client-{}", command.name()));
        let args = command.cli_arg();

        let exit_status = self
            .command
            .start(&args)?
            .wait()
            .await
            .with_context(|| "mithril-client crashed")?;

        if exit_status.success() {
            Ok(output_path)
        } else {
            self.command
                .tail_logs(Some(format!("mithril-client {args:?}").as_str()), 40)
                .await?;

            Err(match exit_status.code() {
                Some(c) => anyhow!("mithril-client exited with code: {c}"),
                None => anyhow!("mithril-client was terminated with a signal"),
            })
        }
    }
}
