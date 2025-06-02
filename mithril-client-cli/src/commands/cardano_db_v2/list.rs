use clap::Parser;

use mithril_client::MithrilResult;

use crate::{
    commands::{cardano_db::CardanoDbListCommand as NewCardanoDbListCommand, SharedArgs},
    CommandContext,
};

/// Clap command to list existing Cardano db snapshots
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbListCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,
}

impl CardanoDbListCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let command = NewCardanoDbListCommand::new_v2(self.shared_args.clone());
        command.execute(context).await
    }
}
