use std::sync::Arc;

use anyhow::Context;
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
        let snapshot_service = dependencies_builder
            .get_snapshot_service()
            .await
            .with_context(|| "Dependencies Builder can not get Snapshot Service")?;
        let items = snapshot_service
            .list()
            .await
            .with_context(|| "Snapshot Service can not get the list of snapshots")?;

        if self.json {
            println!("{}", serde_json::to_string(&items)?);
        } else {
            let items = items
                .into_iter()
                .map(SnapshotListItem::from)
                .collect::<Vec<_>>();
            print_stdout(items.with_title())?;
        }

        Ok(())
    }
}
