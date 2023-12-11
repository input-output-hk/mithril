use anyhow::{anyhow, Context};
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog_scope::logger;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use mithril_client::{ClientBuilder, MessageBuilder};
use mithril_client_cli::{configuration::ConfigParameters, utils::MithrilStakeDistributionUtils};
use mithril_common::StdResult;

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
        let params = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        let aggregator_endpoint = &params.require("aggregator_endpoint")?;
        let genesis_verification_key = &params.require("genesis_verification_key")?;
        let download_dir = &params.require("download_dir")?;
        let download_dir = Path::new(download_dir);
        let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
            .with_logger(logger())
            .build()?;

        let mithril_stake_distribution = client
            .mithril_stake_distribution()
            .get(
                &MithrilStakeDistributionUtils::expand_eventual_artifact_hash_alias(
                    &client,
                    &self.artifact_hash,
                )
                .await?,
            )
            .await?
            .ok_or_else(|| {
                anyhow!(
                    "Mithril stake distribution not found for hash: '{}'",
                    &self.artifact_hash
                )
            })?;

        let certificate = client
            .certificate()
            .verify_chain(&mithril_stake_distribution.certificate_hash)
            .await?;

        let message = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&mithril_stake_distribution)?;

        if !certificate.match_message(&message) {
            return Err(anyhow::anyhow!(
                    "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
                    certificate.signed_message,
                    message.compute_hash()
                ));
        }

        if !download_dir.is_dir() {
            std::fs::create_dir_all(download_dir)?;
        }
        let filepath = PathBuf::new().join(download_dir).join(format!(
            "mithril_stake_distribution-{}.json",
            mithril_stake_distribution.hash
        ));
        std::fs::write(
            &filepath,
            serde_json::to_string(&mithril_stake_distribution).with_context(|| {
                format!(
                    "Can not serialize stake distribution artifact '{:?}'",
                    mithril_stake_distribution
                )
            })?,
        )?;

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
