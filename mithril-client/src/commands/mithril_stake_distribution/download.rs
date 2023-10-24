use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use mithril_common::StdResult;

use mithril_client::dependencies::{ConfigParameters, DependenciesBuilder};

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
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis Verification Key to check the certifiate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl MithrilStakeDistributionDownloadCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config = config_builder
            .set_default("download_dir", ".")?
            .add_source(self.clone())
            .build()?;
        let params: Arc<ConfigParameters> = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        let mut dependencies_builder = DependenciesBuilder::new(params.clone());
        let service = dependencies_builder
            .get_mithril_stake_distribution_service()
            .await
            .with_context(|| {
                "Dependencies Builder can not get Mithril Stake Distribution Service"
            })?;

        let filepath = service
            .download(
                &self.artifact_hash,
                Path::new(&params.require("download_dir")?),
                &params.require("genesis_verification_key")?,
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

impl Source for MithrilStakeDistributionDownloadCommand {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut map = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(download_dir) = self.download_dir.clone() {
            map.insert(
                "download_dir".to_string(),
                Value::new(
                    Some(&namespace),
                    ValueKind::from(download_dir.to_str().ok_or_else(|| {
                        config::ConfigError::Message(format!(
                            "Could not read download directory: '{}'.",
                            download_dir.display()
                        ))
                    })?),
                ),
            );
        }

        if let Some(genesis_verification_key) = self.genesis_verification_key.clone() {
            map.insert(
                "genesis_verification_key".to_string(),
                Value::new(Some(&namespace), ValueKind::from(genesis_verification_key)),
            );
        }

        Ok(map)
    }
}
