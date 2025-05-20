use clap::Parser;

use mithril_client::MithrilResult;

#[derive(Parser, Debug, Clone)]
pub struct SnapshotConverterCommand {}

impl SnapshotConverterCommand {
    /// Main command execution
    pub async fn execute(&self) -> MithrilResult<()> {
        todo!()
    }
}
