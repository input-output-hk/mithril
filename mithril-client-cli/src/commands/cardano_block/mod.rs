//! Commands for the Cardano Block Transaction Snapshot artifact & Cardano Block Proof
//mod certify;
mod snapshot_list;
//mod snapshot_show;

//pub use certify::*;
pub use snapshot_list::*;
//pub use snapshot_show::*;

use crate::CommandContext;
use clap::Subcommand;
use mithril_client::MithrilResult;

/// Cardano blocks management
#[derive(Subcommand, Debug, Clone)]
#[command(about = "Cardano blocks management (alias: cb)")]
pub enum CardanoBlockCommands {
    /// Cardano block commands
    #[clap(subcommand)]
    Snapshot(CardanoBlockSnapshotCommands),
    // /// Certify that a given list of block hashes are included in the Cardano block set
    // #[clap(arg_required_else_help = false)]
    // Certify(CardanoTransactionsCertifyCommand),
}

/// Cardano blocks set
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoBlockSnapshotCommands {
    /// List Cardano block sets
    #[clap(arg_required_else_help = false)]
    List(CardanoBlockSnapshotListCommand),
    // /// Show Cardano block sets
    // #[clap(arg_required_else_help = false)]
    // Show(CardanoTransactionsSnapshotShowCommand),
}

impl CardanoBlockCommands {
    /// Execute Cardano block command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
            // Self::Certify(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoBlockSnapshotCommands {
    /// Execute Cardano block snapshot command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            // Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
