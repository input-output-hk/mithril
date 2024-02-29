//! Commands for the Cardano Transaction Snapshot artifact & Cardano Transactions Proof
mod certify;
mod snapshot_list;
mod snapshot_show;

pub use certify::*;
pub use snapshot_list::*;
pub use snapshot_show::*;

use clap::Subcommand;
use config::builder::DefaultState;
use config::ConfigBuilder;
use mithril_client::MithrilResult;

/// Cardano transactions management
#[derive(Subcommand, Debug, Clone)]
#[command(about = "[unstable] Cardano transactions management (alias: ctx)")]
pub enum CardanoTransactionCommands {
    /// Cardano transaction snapshot commands
    #[clap(subcommand)]
    Snapshot(CardanoTransactionSnapshotCommands),

    /// Certify that a given list of transaction hashes are included in the Cardano transactions set
    #[clap(arg_required_else_help = false)]
    Certify(CardanoTransactionsCertifyCommand),
}

/// Cardano transactions set
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoTransactionSnapshotCommands {
    /// List Cardano transaction sets
    #[clap(arg_required_else_help = false)]
    List(CardanoTransactionSnapshotListCommand),

    /// Show Cardano transaction sets
    #[clap(arg_required_else_help = false)]
    Show(CardanoTransactionsSnapshotShowCommand),
}

impl CardanoTransactionCommands {
    /// Execute Cardano transaction command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
            Self::Certify(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoTransactionSnapshotCommands {
    /// Execute Cardano transaction snapshot command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
