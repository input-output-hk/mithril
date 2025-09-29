//! Commands for the Cardano db artifact
mod download;
mod list;
mod shared_steps;
mod show;
mod verify;

pub use download::*;
pub use list::*;
pub use show::*;
pub use verify::*;

use crate::CommandContext;
use clap::{Subcommand, ValueEnum};
use mithril_client::MithrilResult;

/// Backend to use for Cardano Database commands
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, ValueEnum)]
pub enum CardanoDbCommandsBackend {
    /// Legacy backend
    #[clap(help = "Legacy backend, full database restoration only")]
    V1,
    /// V2 backend
    #[default]
    #[clap(help = "[default] V2 backend, full or partial database restoration")]
    V2,
}

/// Cardano db management (alias: cdb)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbCommands {
    /// Cardano db snapshot commands
    #[clap(subcommand)]
    Snapshot(CardanoDbSnapshotCommands),

    /// Download a Cardano db snapshot and verify its associated certificate
    #[clap(arg_required_else_help = true)]
    Download(CardanoDbDownloadCommand),

    /// Verify a Cardano database content
    #[clap(arg_required_else_help = true)]
    Verify(CardanoDbVerifyCommand),
}

/// Cardano db snapshots
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbSnapshotCommands {
    /// List available Cardano db snapshots
    #[clap(arg_required_else_help = false)]
    List(CardanoDbListCommand),

    /// Show detailed information about a Cardano db snapshot
    #[clap(arg_required_else_help = true)]
    Show(CardanoDbShowCommand),
}

impl CardanoDbCommands {
    /// Execute Cardano db command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self {
            Self::Download(cmd) => cmd.execute(context).await,
            Self::Snapshot(cmd) => cmd.execute(context).await,
            Self::Verify(cmd) => cmd.execute(context).await,
        }
    }
}

impl CardanoDbSnapshotCommands {
    /// Execute Cardano db snapshot command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
