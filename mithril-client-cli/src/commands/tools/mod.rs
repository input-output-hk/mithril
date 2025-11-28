//! Tools commands
//!
//! Provides utility subcommands such as converting restored InMemory UTxO-HD ledger snapshot
//! to different flavors (Legacy, LMDB).

mod aggregator_discovery;
mod snapshot_converter;

pub use aggregator_discovery::*;
pub use snapshot_converter::*;

use anyhow::anyhow;
use clap::Subcommand;
use mithril_client::MithrilResult;

use crate::CommandContext;

/// Tools commands
#[derive(Subcommand, Debug, Clone)]
#[command(about = "Tools commands")]
pub enum ToolsCommands {
    /// UTxO-HD related commands
    #[clap(subcommand, name = "utxo-hd")]
    UTxOHD(UTxOHDCommands),
    /// Aggregator discovery related commands (unstable)
    #[clap(subcommand, name = "aggregator-discovery")]
    AggregatorDiscovery(AggregatorDiscoveryCommands),
}

impl ToolsCommands {
    /// Execute Tools command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self {
            Self::UTxOHD(cmd) => cmd.execute(context).await,
            Self::AggregatorDiscovery(cmd) => {
                if !context.is_unstable_enabled() {
                    Err(anyhow!(Self::unstable_flag_missing_message(
                        "aggregator discovery",
                        "tools"
                    )))
                } else {
                    cmd.execute(context).await
                }
            }
        }
    }

    fn unstable_flag_missing_message(sub_command: &str, command: &str) -> String {
        format!(
            "The \"{}\" subcommand is only accepted using the --unstable flag.\n\n\
            e.g.: \"mithril-client --unstable {} {}\"",
            sub_command, command, sub_command
        )
    }
}

/// UTxO-HD related commands
#[derive(Subcommand, Debug, Clone)]
pub enum UTxOHDCommands {
    /// Convert a restored `InMemory` ledger snapshot to another flavor.
    #[clap(arg_required_else_help = false)]
    SnapshotConverter(SnapshotConverterCommand),
}

impl UTxOHDCommands {
    /// Execute UTxO-HD command
    pub async fn execute(&self, _context: CommandContext) -> MithrilResult<()> {
        match self {
            Self::SnapshotConverter(cmd) => {
                if cfg!(target_os = "linux") && cfg!(target_arch = "aarch64") {
                    return Err(anyhow!(
                        "'snapshot-converter' command is not supported on Linux ARM"
                    ));
                }
                cmd.execute().await
            }
        }
    }
}

/// Aggregator discovery related commands
#[derive(Subcommand, Debug, Clone)]
pub enum AggregatorDiscoveryCommands {
    /// Select an aggregator from the available ones with automatic discovery
    #[clap(arg_required_else_help = false)]
    Select(AggregatorSelectCommand),
}

impl AggregatorDiscoveryCommands {
    /// Execute Aggregator discovery command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self {
            Self::Select(cmd) => cmd.execute(&context).await,
        }
    }
}
