use std::{collections::HashMap, sync::Arc};

use anyhow::Context;
use clap::Parser;
use cli_table::{print_stdout, WithTitle};

use config::{builder::DefaultState, ConfigBuilder};
use mithril_client::{
    common::StdResult,
    dependencies::{ConfigParameters, DependenciesBuilder},
    SnapshotListItem,
};

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
        let config = config_builder.build()?;
        let params: Arc<ConfigParameters> = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        let mut dependencies_builder = DependenciesBuilder::new(params);
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
