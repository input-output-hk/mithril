use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};

use mithril_common::StdResult;

/// Cardano transaction commitment LIST command
#[derive(Parser, Debug, Clone)]
pub struct CardanoTransactionCommitmentListCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl CardanoTransactionCommitmentListCommand {
    /// Main command execution
    pub async fn execute(&self, _config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        Ok(())
    }
}
