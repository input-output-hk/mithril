use std::sync::Arc;

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, Config, ConfigBuilder};

use mithril_common::StdResult;

use crate::{dependencies::DependenciesBuilder, SnapshotListItem};

/// Clap command to list existing snapshots
#[derive(Parser, Debug, Clone)]
pub struct SnapshotListCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl SnapshotListCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config: Config = config_builder.build()?;
        let mut dependencies_builder = DependenciesBuilder::new(Arc::new(config));
        let snapshot_service = dependencies_builder.get_snapshot_service().await?;
        let signed_entities = snapshot_service.list().await?;

        if self.json {
            println!("{}", serde_json::to_string(&signed_entities)?);
        } else {
            let snapshot_list_items: Vec<SnapshotListItem> = signed_entities
                .into_iter()
                .map(|snapshot| snapshot.into())
                .collect();
            print_stdout(snapshot_list_items.with_title()).unwrap();
        }

        Ok(())
    }
}
