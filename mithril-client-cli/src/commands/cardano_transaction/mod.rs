//! Commands for the Cardano Transaction Commitment artifact & Cardano Transactions Proof
mod list;

pub use list::*;

use clap::Subcommand;
use config::builder::DefaultState;
use config::ConfigBuilder;
use mithril_common::StdResult;

/// Cardano transactions management (alias: ctx)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoTransactionCommands {
    /// List Cardano transaction commitments
    #[clap(arg_required_else_help = false)]
    List(CardanoTransactionCommitmentListCommand),
}

impl CardanoTransactionCommands {
    /// Execute Mithril stake distribution command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
        }
    }
}
