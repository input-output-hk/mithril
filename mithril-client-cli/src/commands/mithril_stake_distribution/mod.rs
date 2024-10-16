//! Commands for the Mithril Stake Distribution artifact
mod download;
mod list;

pub use download::*;
pub use list::*;

use crate::CommandContext;
use clap::Subcommand;
use mithril_client::MithrilResult;

/// Mithril Stake Distribution management (alias: msd)
#[derive(Subcommand, Debug, Clone)]
pub enum MithrilStakeDistributionCommands {
    /// List certified stake distributions
    #[clap(arg_required_else_help = false)]
    List(MithrilStakeDistributionListCommand),

    /// Download and verify the given Mithril Stake Distribution
    #[clap(arg_required_else_help = false)]
    Download(MithrilStakeDistributionDownloadCommand),
}

impl MithrilStakeDistributionCommands {
    /// Execute Mithril stake distribution command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
        }
    }
}
