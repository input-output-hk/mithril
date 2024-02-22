//! Commands for the Cardano Transaction Commitment artifact & Cardano Transactions Proof
mod certify;
mod commitment_list;
mod commitment_show;

pub use certify::*;
pub use commitment_list::*;
pub use commitment_show::*;

use clap::Subcommand;
use config::builder::DefaultState;
use config::ConfigBuilder;
use mithril_client::MithrilResult;

/// Cardano transactions management
#[derive(Subcommand, Debug, Clone)]
#[command(about = "[unstable] Cardano transactions management (alias: ctx)")]
pub enum CardanoTransactionCommands {
    /// Cardano transaction commitment commands
    #[clap(subcommand)]
    Commitment(CardanoTransactionCommitmentCommands),

    /// Certify that a given list of transaction hashes are included in the Cardano transactions set
    #[clap(arg_required_else_help = false)]
    Certify(CardanoTransactionsCertifyCommand),
}

/// Cardano transactions set
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoTransactionCommitmentCommands {
    /// List Cardano transaction sets
    #[clap(arg_required_else_help = false)]
    List(CardanoTransactionCommitmentListCommand),

    /// Show Cardano transaction sets
    #[clap(arg_required_else_help = false)]
    Show(CardanoTransactionsCommitmentShowCommand),
}

impl CardanoTransactionCommands {
    /// Execute Cardano transaction command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::Commitment(cmd) => cmd.execute(config_builder).await,
            Self::Certify(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoTransactionCommitmentCommands {
    /// Execute Cardano transaction commitment command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
