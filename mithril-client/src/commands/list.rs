use std::{error::Error, sync::Arc};

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    api_version::APIVersionProvider,
    entities::Epoch,
    era::{EraChecker, SupportedEra},
};
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
    ) -> Result<(), Box<dyn Error>> {
        debug!("List snapshots");
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("{:?}", config);
        let runtime = Runtime::new(config.network.clone());
        // TODO: This does not allow the client to handle an upgraded API version of a new supported era after the switch
        // In order to do so, we should retrieve the list of supported versions from the API Version provider and test them sequentially until one hopefully succeeds
        let era_checker = Arc::new(EraChecker::new(
            SupportedEra::eras().first().unwrap().to_owned(),
            Epoch(0),
        ));
        let api_version_provider = Arc::new(APIVersionProvider::new(era_checker.clone()));
        let aggregator_handler = AggregatorHTTPClient::new(
            config.network.clone(),
            config.aggregator_endpoint,
            api_version_provider,
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
