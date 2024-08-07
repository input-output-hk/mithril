//! Commands for the Cardano Stake Distribution artifact
mod download;
mod list;

pub use download::*;
pub use list::*;

use clap::Subcommand;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_client::MithrilResult;

/// Cardano Stake Distribution management (alias: csd)
#[derive(Subcommand, Debug, Clone)]
#[command(about = "[unstable] Cardano stake distribution management (alias: csd)")]
pub enum CardanoStakeDistributionCommands {
    /// List certified Cardano Stake Distributions
    #[clap(arg_required_else_help = false)]
    List(CardanoStakeDistributionListCommand),

    /// Download and verify the given Cardano Stake Distribution
    #[clap(arg_required_else_help = true)]
    Download(CardanoStakeDistributionDownloadCommand),
}

impl CardanoStakeDistributionCommands {
    /// Execute Cardano Stake Distribution command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
        }
    }
}
