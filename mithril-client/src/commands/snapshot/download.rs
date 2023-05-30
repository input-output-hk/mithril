use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::{
    api_version::APIVersionProvider, certificate_chain::MithrilCertificateVerifier,
    digesters::CardanoImmutableDigester, StdResult,
};

use crate::{
    aggregator_client::{AggregatorHTTPClient, CertificateClient, SnapshotClient},
    services::{MithrilClientSnapshotService, SnapshotService},
    Config,
};

/// Clap command to download the snapshot and verify the certificate.
#[derive(Parser, Debug, Clone)]
pub struct SnapshotDownloadCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Digest of the snapshot to download. Use the `list` command to get that information.
    digest: String,

    /// Directory where the snapshot will be downloaded. By default, a
    /// subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long, default_value = ".")]
    download_dir: PathBuf,
}

impl SnapshotDownloadCommand {
    /// Command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
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
                    &self.download_dir,
                    None,
                    slog_scope::logger(),
                )),
            )
        };
        let filepath = snapshot_service
            .download(
                &self.digest,
                &self.download_dir,
                &config.genesis_verification_key,
            )
            .await?;

        if self.json {
            println!(r#"{{"file": "{}"}}"#, filepath.display());
        } else {
            println!("Download snapshot: '{}'.", filepath.display());
        }

        Ok(())
    }
}
