//! Commands for the Cardano Transaction Commitment artifact & Cardano Transactions Proof
mod certify;
mod sets_list;
mod sets_show;

pub use certify::*;
pub use sets_list::*;
pub use sets_show::*;

use clap::Subcommand;
use config::builder::DefaultState;
use config::ConfigBuilder;
use mithril_common::StdResult;

/// Cardano transactions management (alias: ctx)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoTransactionCommands {
    /// Cardano transaction sets commands
    #[clap(subcommand)]
    Sets(CardanoTransactionSetsCommands),

    /// Certify that a given list of transaction hashes are included in the Cardano transactions set
    #[clap(arg_required_else_help = false)]
    Certify(CardanoTransactionsCertifyCommand),
}

/// Cardano transactions set
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoTransactionSetsCommands {
    /// List Cardano transaction sets
    #[clap(arg_required_else_help = false)]
    List(CardanoTransactionSetsListCommand),

    /// Show Cardano transaction sets
    #[clap(arg_required_else_help = false)]
    Show(CardanoTransactionsSetsShowCommand),
}

impl CardanoTransactionCommands {
    /// Execute Cardano transaction command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::Sets(cmd) => cmd.execute(config_builder).await,
            Self::Certify(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoTransactionSetsCommands {
    /// Execute Cardano transaction sets command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
