use std::{path::Path, sync::Arc};

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::debug;

use mithril_common::{
    api_version::APIVersionProvider, certificate_chain::MithrilCertificateVerifier,
    digesters::CardanoImmutableDigester, StdResult,
};

use crate::{
    aggregator_client::{AggregatorHTTPClient, CertificateClient, SnapshotClient},
    services::{MithrilClientSnapshotService, SnapshotService},
    Config, SnapshotListItem,
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
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("{:?}", config);
        let snapshot_service = {
            let http_client = Arc::new(AggregatorHTTPClient::new(
                &config.aggregator_endpoint,
                APIVersionProvider::compute_all_versions_sorted()?,
            ));

            MithrilClientSnapshotService::new(
                Arc::new(SnapshotClient::new(http_client.clone())),
                Arc::new(CertificateClient::new(http_client)),
                Arc::new(MithrilCertificateVerifier::new(slog_scope::logger())),
                Arc::new(CardanoImmutableDigester::new(
                    // the digester needs a path so we pass an empty path since we do not need the digester here
                    Path::new(""),
                    None,
                    slog_scope::logger(),
                )),
            )
        };

        let snapshots = snapshot_service.list().await?;

        if self.json {
            println!("{}", serde_json::to_string(&snapshots)?);
        } else {
            let snapshot_list_items: Vec<SnapshotListItem> = snapshots
                .into_iter()
                .map(|snapshot| snapshot.into())
                .collect();
            print_stdout(snapshot_list_items.with_title()).unwrap();
        }

        Ok(())
    }
}
