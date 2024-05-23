//! Commands for the cardano db artifact
mod download;
mod list;
mod show;

pub use download::*;
pub use list::*;
pub use show::*;

use clap::Subcommand;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_client::MithrilResult;

/// Cardano db management (alias: cdb)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbCommands {
    /// Cardano db snapshot commands
    #[clap(subcommand)]
    Snapshot(CardanoDbSnapshotCommands),

    /// Download a Cardano db snapshot and verify its associated certificate
    #[clap(arg_required_else_help = true)]
    Download(CardanoDbDownloadCommand),
}

/// Cardano db snapshots
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbSnapshotCommands {
    /// List available cardano db snapshots
    #[clap(arg_required_else_help = false)]
    List(CardanoDbListCommand),

    /// Show detailed information about a cardano db snapshot
    #[clap(arg_required_else_help = true)]
    Show(CardanoDbShowCommand),
}

impl CardanoDbCommands {
    /// Execute cardano db command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::Download(cmd) => cmd.execute(config_builder).await,
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoDbSnapshotCommands {
    /// Execute Cardano db snapshot command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
