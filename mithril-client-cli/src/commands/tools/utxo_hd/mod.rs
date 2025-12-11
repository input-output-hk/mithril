//! Commands for UTxO-HD
mod snapshot_converter;

pub use snapshot_converter::*;

use anyhow::anyhow;
use clap::Subcommand;

use mithril_client::MithrilResult;

use crate::CommandContext;

/// UTxO-HD related commands
#[derive(Subcommand, Debug, Clone)]
pub enum UTxOHDCommands {
    /// Convert a restored `InMemory` ledger snapshot to another flavor.
    #[clap(arg_required_else_help = false)]
    SnapshotConverter(SnapshotConverterCommand),
}

impl UTxOHDCommands {
    /// Execute UTxO-HD command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self {
            Self::SnapshotConverter(cmd) => {
                if cfg!(target_os = "linux") && cfg!(target_arch = "aarch64") {
                    return Err(anyhow!(
                        "'snapshot-converter' command is not supported on Linux ARM"
                    ));
                }
                cmd.execute(context).await
            }
        }
    }
}
