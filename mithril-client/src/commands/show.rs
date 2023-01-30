use std::{error::Error, sync::Arc};

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::debug;

use crate::{convert_to_field_items, AggregatorHTTPClient, Config, Runtime};

/// Show detailed information about a snapshot.
#[derive(Parser, Debug, Clone)]
pub struct ShowCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Digest of the snapshot to download. Use the `list` command to get that information.
    digest: String,
}

impl ShowCommand {
    /// call the runtime list function
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("Show snapshot");
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("{:?}", config);
        let runtime = Runtime::new(config.network.clone());
        let aggregator_handler =
            AggregatorHTTPClient::new(config.network.clone(), config.aggregator_endpoint);
        let snapshot = runtime
            .show_snapshot(Arc::new(aggregator_handler), &self.digest)
            .await?;

        if self.json {
            println!("{}", serde_json::to_string(&snapshot).unwrap());
        } else {
            print_stdout(convert_to_field_items(&snapshot, config.network.clone()).with_title())
                .unwrap();
        }

        Ok(())
    }
}
