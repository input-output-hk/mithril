//! Commands for the Cardano Transaction Commitment artifact & Cardano Transactions Proof
mod sets_list;

pub use sets_list::*;

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
}

/// Cardano transactions set
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoTransactionSetsCommands {
    /// List Cardano transaction commitments
    #[clap(arg_required_else_help = false)]
    List(CardanoTransactionSetsListCommand),
}

impl CardanoTransactionCommands {
    /// Execute Cardano transaction command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            CardanoTransactionCommands::Sets(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoTransactionSetsCommands {
    /// Execute Cardano transaction sets command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
        }
    }
}
