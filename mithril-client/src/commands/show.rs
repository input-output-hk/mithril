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
