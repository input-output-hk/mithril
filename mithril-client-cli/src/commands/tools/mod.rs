//! Tools commands
//!
//! Provides utility subcommands such as converting restored InMemory UTxO-HD ledger snapshot
//! to different flavors (Legacy, LMDB).

mod snapshot_converter;

pub use snapshot_converter::*;

use anyhow::anyhow;
use clap::Subcommand;
use mithril_client::MithrilResult;

/// Tools commands
#[derive(Subcommand, Debug, Clone)]
#[command(about = "Tools commands")]
pub enum ToolsCommands {
    /// UTxO-HD related commands
    #[clap(subcommand, name = "utxo-hd")]
    UTxOHD(UTxOHDCommands),
}

impl ToolsCommands {
    /// Execute Tools command
    pub async fn execute(&self) -> MithrilResult<()> {
        match self {
            Self::UTxOHD(cmd) => cmd.execute().await,
        }
    }
}

/// UTxO-HD related commands
#[derive(Subcommand, Debug, Clone)]
pub enum UTxOHDCommands {
    /// Convert a restored `InMemory` ledger snapshot to another flavor.
    #[clap(arg_required_else_help = false)]
    SnapshotConverter(SnapshotConverterCommand),
}

impl UTxOHDCommands {
    /// Execute UTxO-HD command
    pub async fn execute(&self) -> MithrilResult<()> {
        match self {
            Self::SnapshotConverter(cmd) => {
                if cfg!(target_os = "linux") && cfg!(target_arch = "aarch64") {
                    return Err(anyhow!(
                        "'snapshot-converter' command is not supported on Linux ARM"
                    ));
                }
                cmd.execute().await
            }
        }
    }
}
