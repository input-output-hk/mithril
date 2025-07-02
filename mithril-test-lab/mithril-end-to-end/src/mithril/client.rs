use crate::utils::MithrilCommand;
use crate::{ANCILLARY_MANIFEST_VERIFICATION_KEY, GENESIS_VERIFICATION_KEY};
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
                vec![
                    "download".to_string(),
                    "--include-ancillary".to_string(),
                    "--download-dir".to_string(),
                    "v1".to_string(),
                    digest.clone(),
                ]
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
                vec![
                    "download".to_string(),
                    "--include-ancillary".to_string(),
                    "--download-dir".to_string(),
                    "v2".to_string(),
                    hash.clone(),
                ]
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
            ClientCommand::CardanoDb(cmd) => [
                vec!["cardano-db".to_string()],
                cmd.cli_arg(),
                vec!["--backend".to_string(), "v1".to_string()],
            ]
            .concat(),
            ClientCommand::MithrilStakeDistribution(cmd) => {
                [vec!["mithril-stake-distribution".to_string()], cmd.cli_arg()].concat()
            }
            ClientCommand::CardanoTransaction(cmd) => {
                [vec!["cardano-transaction".to_string()], cmd.cli_arg()].concat()
            }
            ClientCommand::CardanoStakeDistribution(cmd) => {
                [vec!["cardano-stake-distribution".to_string()], cmd.cli_arg()].concat()
            }
            ClientCommand::CardanoDbV2(cmd) => [
                vec!["cardano-db".to_string()],
                cmd.cli_arg(),
                vec!["--backend".to_string(), "v2".to_string()],
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
            ("GENESIS_VERIFICATION_KEY", GENESIS_VERIFICATION_KEY),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
            (
                "ANCILLARY_VERIFICATION_KEY",
                ANCILLARY_MANIFEST_VERIFICATION_KEY,
            ),
        ]);
        let args = vec!["-vvv", "--origin-tag", "E2E"];
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
