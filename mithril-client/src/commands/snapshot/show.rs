use std::{path::Path, sync::Arc};

use clap::Parser;
use cli_table::{print_stdout, WithTitle};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    api_version::APIVersionProvider, certificate_chain::MithrilCertificateVerifier,
    digesters::CardanoImmutableDigester, entities::Snapshot, StdError,
};

use crate::{
    aggregator_client::{AggregatorHTTPClient, CertificateClient, SnapshotClient},
    services::{MithrilClientSnapshotService, SnapshotService},
    Config, SnapshotFieldItem,
};

/// Clap command to show a given snapshot
#[derive(Parser, Debug, Clone)]
pub struct SnapshotShowCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Snapshot digest
    digest: String,
}

impl SnapshotShowCommand {
    /// Snapshot Show command
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), StdError> {
        let config: Config = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
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
                    &Path::new(""),
                    None,
                    slog_scope::logger(),
                )),
            )
        };
        let snapshot = snapshot_service.show(&self.digest).await?;

        if self.json {
            println!("{}", serde_json::to_string(&snapshot)?);
        } else {
            print_stdout(convert_to_field_items(&snapshot).with_title()).unwrap();
        }

        Ok(())
    }
}

/// Convert Snapshot to SnapshotFieldItems routine
fn convert_to_field_items(snapshot: &Snapshot) -> Vec<SnapshotFieldItem> {
    let mut field_items = vec![
        SnapshotFieldItem::new("Epoch".to_string(), format!("{}", snapshot.beacon.epoch)),
        SnapshotFieldItem::new(
            "Immutable File Number".to_string(),
            format!("{}", snapshot.beacon.immutable_file_number),
        ),
        SnapshotFieldItem::new("Digest".to_string(), snapshot.digest.to_string()),
        SnapshotFieldItem::new("Size".to_string(), format!("{}", snapshot.size)),
    ];
    for (idx, location) in snapshot.locations.iter().enumerate() {
        field_items.push(SnapshotFieldItem::new(
            format!("Location {}", idx + 1),
            location.to_string(),
        ));
    }
    field_items.push(SnapshotFieldItem::new(
        "Created".to_string(),
        snapshot.created_at.to_string(),
    ));
    field_items
}
