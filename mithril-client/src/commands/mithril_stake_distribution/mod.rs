//! Commands for the Mithril Stake Distribution artifact
mod download;
mod list;

pub use download::*;
pub use list::*;

use clap::Subcommand;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::StdResult;

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
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
        }
    }
}
