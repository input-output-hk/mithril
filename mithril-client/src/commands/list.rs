use std::sync::Arc;

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{api_version::APIVersionProvider, StdError};
use slog_scope::debug;

use crate::{AggregatorHTTPClient, Config, Runtime};

/// List signed snapshots from an Aggregator.
#[derive(Parser, Debug, Clone)]
pub struct ListCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl ListCommand {
    /// call the runtime list function
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), StdError> {
        debug!("List snapshots");
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("{:?}", config);
        let runtime = Runtime::new(config.network.clone());
        let aggregator_handler = AggregatorHTTPClient::new(
            config.network.clone(),
            config.aggregator_endpoint,
            APIVersionProvider::compute_all_versions_sorted()?,
        );
        let snapshot_list_items = runtime.list_snapshots(Arc::new(aggregator_handler)).await?;

        if self.json {
            println!("{}", serde_json::to_string(&snapshot_list_items).unwrap());
        } else {
            print_stdout(snapshot_list_items.with_title()).unwrap();
        }

        Ok(())
    }
}
