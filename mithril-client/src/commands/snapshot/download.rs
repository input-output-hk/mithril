use std::{path::PathBuf, sync::Arc};

use clap::Parser;
use config::{builder::DefaultState, Config, ConfigBuilder};
use mithril_common::StdResult;

use crate::dependencies::DependenciesBuilder;

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
        let config_builder = config_builder.set_default("genesis_verification_key", "")?;
        let config: Config = config_builder.build()?;
        let config = Arc::new(config);
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let snapshot_service = dependencies_builder.get_snapshot_service().await?;
        let snapshot = snapshot_service.show(&self.digest).await?;
        let filepath = snapshot_service
            .download(
                &snapshot,
                &self.download_dir,
                &config.get_string("genesis_verification_key")?,
            )
            .await?;

        if self.json {
            println!(
                r#"{{"db_directory": "{}"}}"#,
                filepath.canonicalize()?.display()
            );
        } else {
            println!(
                r###"Unpack success snapshot '{}'
into {}

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} inputoutput/cardano-node:8.1.1

"###,
                &self.digest,
                filepath.display(),
                filepath.display(),
                snapshot.beacon.network,
            );
        }

        Ok(())
    }
}
