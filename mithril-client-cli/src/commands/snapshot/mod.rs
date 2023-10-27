//! Commands for the snapshot artifact
mod download;
mod list;
mod show;

pub use download::*;
pub use list::*;
pub use show::*;

use clap::Subcommand;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::StdResult;

/// Snapshot management
#[derive(Subcommand, Debug, Clone)]
pub enum SnapshotCommands {
    /// List available snapshots
    #[clap(arg_required_else_help = false)]
    List(SnapshotListCommand),

    /// Show detailed informations about a snapshot
    #[clap(arg_required_else_help = true)]
    Show(SnapshotShowCommand),

    /// Download the snapshot and verify the certificate
    #[clap(arg_required_else_help = true)]
    Download(SnapshotDownloadCommand),
}

impl SnapshotCommands {
    /// Execute snapshot command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
