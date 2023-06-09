//! Commands for the Mithril Stake Distribution artifact
mod download;
mod list;

use clap::Subcommand;
use config::{builder::DefaultState, ConfigBuilder};

pub use download::*;
pub use list::*;
use mithril_common::StdError;

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
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), StdError> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
        }
    }
}
