use std::{error::Error, fmt::Display, sync::Arc};

use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use serde::Serialize;
use slog_scope::debug;

use crate::{AggregatorHTTPClient, Config, Runtime};

/// Download a snapshot.
#[derive(Parser, Debug, Clone)]
pub struct DownloadCommand {
    /// Digest of the snapshot to download. Use the `list` command to get that information.
    digest: String,

    /// Location index of the snapshot.
    #[clap(short = 'i', long, default_value = "1")]
    location_index: isize,

    /// Enable JSON output.
    #[clap(short, long)]
    json: bool,
}

impl DownloadCommand {
    /// call the runtime download function
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("Download snapshots");
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {}", e))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {}", e))?;
        debug!("{:?}", config);
        let runtime = Runtime::new(config.network.clone());
        let aggregator_handler =
            AggregatorHTTPClient::new(config.network.clone(), config.aggregator_endpoint);
        let (from, to) = runtime
            .download_snapshot(
                Arc::new(aggregator_handler),
                &self.digest,
                self.location_index,
            )
            .await?;
        let token = OutputToken::new(self.digest.clone(), self.location_index, from, to);

        let output = if self.json {
            serde_json::to_string(&token)?
        } else {
            format!("success!\n{}", token)
        };
        println!("{}", output);
        Ok(())
    }
}

#[derive(Serialize)]
struct OutputToken {
    digest: String,
    index: isize,
    from: String,
    to: String,
}

impl OutputToken {
    fn new(digest: String, index: isize, from: String, to: String) -> Self {
        Self {
            digest,
            index,
            from,
            to,
        }
    }
}

impl Display for OutputToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "digest: {}\nindex: {}\nfrom: {}, to: {}",
            self.digest, self.index, self.from, self.to
        )
    }
}
