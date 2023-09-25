use std::{path::PathBuf, sync::Arc};

use anyhow::Context;
use clap::Parser;
use config::{builder::DefaultState, Config, ConfigBuilder};
use mithril_common::StdResult;

use crate::dependencies::DependenciesBuilder;

/// Download and verify a Mithril Stake Distribution information. If the
/// verification fails, the file is not persisted.
#[derive(Parser, Debug, Clone)]
pub struct MithrilStakeDistributionDownloadCommand {
    /// Hash of the Mithril Stake Distribution artifact.
    ///
    /// If `latest` is specified as artifact_hash, the command will return the latest stake distribution.
    artifact_hash: String,

    /// Directory where the Mithril Stake Distribution will be downloaded. By default, a
    /// subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long, default_value = ".")]
    download_dir: PathBuf,
}

impl MithrilStakeDistributionDownloadCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config_builder = config_builder
            .set_default("genesis_verification_key", "")
            .unwrap();
        let config: Config = config_builder.build()?;
        let config = Arc::new(config);
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let service = dependencies_builder
            .get_mithril_stake_distribution_service()
            .await
            .with_context(|| {
                "Dependencies Builder can not get Mithril Stake Distribution Service"
            })?;

        let filepath = service
            .download(
                &self.artifact_hash,
                &self.download_dir,
                &config.get_string("genesis_verification_key")?,
            )
            .await
            .with_context(|| {
                format!(
                    "Mithril Stake Distribution Service can not download and verify the artifact for hash: '{}'",
                    self.artifact_hash
                )
            })?;

        println!(
            "Mithril Stake Distribution '{}' has been verified and saved as '{}'.",
            self.artifact_hash,
            filepath.display()
        );

        Ok(())
    }
}
