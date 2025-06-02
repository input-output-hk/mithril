use clap::Parser;

use crate::{
    commands::{cardano_db::CardanoDbShowCommand as NewCardanoDbShowCommand, SharedArgs},
    CommandContext,
};

use mithril_client::MithrilResult;

/// Clap command to show a given Cardano db
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbShowCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Hash of the Cardano db snapshot to show or `latest` for the latest artifact
    hash: String,
}

impl CardanoDbShowCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Cardano DB snapshot Show command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let command = NewCardanoDbShowCommand::new_v2(self.shared_args.clone(), self.hash.clone());
        command.execute(context).await
    }
}
