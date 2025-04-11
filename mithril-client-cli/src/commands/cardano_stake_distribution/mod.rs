//! Commands for the Cardano stake distribution artifact
mod download;
mod list;

pub use download::*;
pub use list::*;

use crate::CommandContext;
use clap::Subcommand;
use mithril_client::MithrilResult;

/// Cardano stake distribution management (alias: csd)
#[derive(Subcommand, Debug, Clone)]
#[command(about = "Cardano stake distribution management (alias: csd)")]
pub enum CardanoStakeDistributionCommands {
    /// List certified Cardano stake distributions
    #[clap(arg_required_else_help = false)]
    List(CardanoStakeDistributionListCommand),

    /// Download and verify the given Cardano stake distribution
    #[clap(arg_required_else_help = true)]
    Download(CardanoStakeDistributionDownloadCommand),
}

impl CardanoStakeDistributionCommands {
    /// Execute Cardano stake distribution command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
        }
    }
}
