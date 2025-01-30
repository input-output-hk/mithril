//! Commands for the cardano db v2 artifact
mod list;
mod show;

pub use list::*;
pub use show::*;

use crate::CommandContext;
use clap::Subcommand;
use mithril_client::MithrilResult;

/// Cardano db v2 management (alias: cdbv2)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbV2Commands {
    /// Cardano db snapshot v2 commands
    #[clap(subcommand)]
    Snapshot(CardanoDbV2SnapshotCommands),
}

/// Cardano db v2 snapshots
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbV2SnapshotCommands {
    /// List available cardano db v2 snapshots
    #[clap(arg_required_else_help = false)]
    List(CardanoDbListCommand),

    /// Show detailed information about a cardano db v2 snapshot
    #[clap(arg_required_else_help = true)]
    Show(CardanoDbShowCommand),
}

impl CardanoDbV2Commands {
    /// Execute Cardano db v2 command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoDbV2SnapshotCommands {
    /// Execute Cardano db v2 snapshot command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
