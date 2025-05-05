//! Commands for the Cardano db artifact
mod download;
mod list;
mod show;

use std::sync::Arc;

pub use download::*;
pub use list::*;
pub use show::*;

use crate::CommandContext;
use clap::Subcommand;
use mithril_client::MithrilResult;

/// Cardano db management (alias: cdb)
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbCommands {
    /// Cardano db snapshot commands
    #[clap(subcommand)]
    Snapshot(CardanoDbSnapshotCommands),

    /// Download a Cardano db snapshot and verify its associated certificate
    #[clap(arg_required_else_help = true)]
    Download(CardanoDbDownloadCommand),
}

/// Cardano db snapshots
#[derive(Subcommand, Debug, Clone)]
pub enum CardanoDbSnapshotCommands {
    /// List available Cardano db snapshots
    #[clap(arg_required_else_help = false)]
    List(CardanoDbListCommand),

    /// Show detailed information about a Cardano db snapshot
    #[clap(arg_required_else_help = true)]
    Show(CardanoDbShowCommand),
}

impl CardanoDbCommands {
    /// Execute Cardano db command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::Download(cmd) => {
                cmd.execute(config_builder, Arc::new(AncillaryLogMessageImpl {}))
                    .await
            }
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
        }
    }
}

impl CardanoDbSnapshotCommands {
    /// Execute Cardano db snapshot command
    pub async fn execute(&self, config_builder: CommandContext) -> MithrilResult<()> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
        }
    }
}

#[cfg_attr(test, mockall::automock)]
/// A trait to provide a print messages related to the ancillary files
pub trait AncillaryLogMessage {
    /// Logs a warning message indicating that fast bootstrap is not available.
    fn warn_fast_bootstrap_not_available(&self);
    /// Logs a warning message indicating that the ancillary signature is not done by Mithril.
    fn warn_ancillary_signature_not_signed_by_mithril(&self);
}
struct AncillaryLogMessageImpl {}

impl AncillaryLogMessage for AncillaryLogMessageImpl {
    /// This method provides guidance on how to enable fast bootstrap by including ancillary files
    fn warn_fast_bootstrap_not_available(&self) {
        println!("The fast bootstrap of the Cardano node is not available with the current parameters used in this command: this means that the ledger state will be recomputed from genesis at startup of the Cardano node.

    In order to activate the fast bootstrap of the Cardano node, add the following parameters to the command:

        --include-ancillary
        and --ancillary-verification-key (or environment variable ANCILLARY_VERIFICATION_KEY). Caution: The ancillary files, including the ledger state, are not currently signed by Mithril. As a mitigation, IOG owned keys are used to sign these files. For more information, please refer to the network configuration page of the documentation (https://mithril.network/doc/manual/getting-started/network-configurations).");
    }

    fn warn_ancillary_signature_not_signed_by_mithril(&self) {
        println!("Ancillary verification does not use the Mithril certification: as a mitigation, IOG owned keys are used to sign these files.");
    }
}
