use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    api_version::APIVersionProvider, certificate_chain::MithrilCertificateVerifier, StdResult,
};

use crate::{
    aggregator_client::{AggregatorHTTPClient, CertificateClient, SnapshotClient},
    services::{MithrilClientSnapshotService, SnapshotConfig, SnapshotService},
};

/// Clap command to download the snapshot and verify the certificate.
#[derive(Parser, Debug, Clone)]
pub struct SnapshotDownloadCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    digest: String,

    #[clap(long, default_value = ".")]
    dirpath: PathBuf,
}

impl SnapshotDownloadCommand {
    /// Command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config: SnapshotConfig = config_builder
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
            )
        };
        let filepath = snapshot_service
            .download(&self.digest, self.dirpath.as_path(), "")
            .await?;

        if self.json {
            println!(r#"{{"file": "{}"}}"#, filepath.display());
        } else {
            println!("Download snapshot: '{}'.", filepath.display());
        }

        Ok(())
    }
}
