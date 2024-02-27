//! Commands for the cardano db artifact
mod download;
mod list;
mod show;

pub use download::*;
pub use list::*;
pub use show::*;

use clap::Subcommand;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_client::MithrilResult;

/// Cardano db management (alias: cdb)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbCommands {
    /// List available cardano dbs
    #[clap(arg_required_else_help = false)]
    List(CardanoDbListCommand),

    /// Show detailed informations about a cardano db
    #[clap(arg_required_else_help = true)]
    Show(CardanoDbShowCommand),

    /// Download the Cardano db and verify the certificate
    #[clap(arg_required_else_help = true)]
    Download(CardanoDbDownloadCommand),
}

impl CardanoDbCommands {
    /// Execute cardano db command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}
