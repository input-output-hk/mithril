use clap::{Subcommand, CommandFactory};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{StdResult, generate_doc::GenerateDocCommands};
use super::{AggregatorCommand, PassiveCommand, SignerCommand, Args};

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

    #[clap(alias("doc"))]
    GenerateDoc(GenerateDocCommands),
}

impl RelayCommands {
    /// Execute the command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::Aggregator(cmd) => cmd.execute(config_builder).await,
            Self::Signer(cmd) => cmd.execute(config_builder).await,
            Self::Passive(cmd) => cmd.execute(config_builder).await,
            Self::GenerateDoc(cmd) => cmd.execute(&mut Args::command()),
        }
    }
}
