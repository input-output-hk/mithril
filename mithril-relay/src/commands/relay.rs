use anyhow::anyhow;
use clap::{CommandFactory, Subcommand};
use mithril_common::StdResult;
use mithril_doc::GenerateDocCommands;

use super::{AggregatorCommand, Args, CommandContext, PassiveCommand, SignerCommand};

/// The available sub-commands of the relay
#[derive(Subcommand, Debug, Clone)]
pub enum RelayCommands {
    /// Run a relay for a Mithril aggregator
    #[clap(arg_required_else_help = true)]
    Aggregator(AggregatorCommand),

    /// Run a relay for a Mithril signer
    #[clap(arg_required_else_help = true)]
    Signer(SignerCommand),

    /// Run a passive relay (just a peer in the P2P network)
    #[clap(arg_required_else_help = true)]
    Passive(PassiveCommand),

    /// Generate command line documentation
    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}

impl RelayCommands {
    /// Execute the command
    pub async fn execute(&self, context: CommandContext) -> StdResult<()> {
        match self {
            Self::Aggregator(cmd) => cmd.execute(context).await,
            Self::Signer(cmd) => cmd.execute(context).await,
            Self::Passive(cmd) => cmd.execute(context).await,
            Self::GenerateDoc(cmd) => cmd
                .execute(&mut Args::command())
                .map_err(|message| anyhow!(message)),
        }
    }
}
