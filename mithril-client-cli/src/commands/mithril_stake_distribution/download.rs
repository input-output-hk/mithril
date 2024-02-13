use anyhow::Context;
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog_scope::logger;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{configuration::ConfigParameters, utils::ExpanderUtils};
use mithril_client::{ClientBuilder, MessageBuilder};
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
        let download_dir = &params.require("download_dir")?;
        let download_dir = Path::new(download_dir);
        let client = ClientBuilder::aggregator(
            &params.require("aggregator_endpoint")?,
            &params.require("genesis_verification_key")?,
        )
        .with_logger(logger())
        .build()?;

        let get_list_of_artifact_ids = || async {
            let mithril_stake_distributions = client.mithril_stake_distribution().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest stake distribution hash"
            })?;

            Ok(mithril_stake_distributions
                .iter()
                .map(|msd| msd.hash.to_owned())
                .collect::<Vec<String>>())
        };
        let mithril_stake_distribution = client
            .mithril_stake_distribution()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(
                    &self.artifact_hash,
                    get_list_of_artifact_ids(),
                )
                .await?,
            )
            .await?
            .with_context(|| {
                format!(
                    "Can not download and verify the artifact for hash: '{}'",
                    self.artifact_hash
                )
            })?;

        let certificate = client
            .certificate()
            .verify_chain(&mithril_stake_distribution.certificate_hash)
            .await
            .with_context(|| {
                format!(
                    "Can not verify the certificate chain from certificate_hash: '{}'",
                    &mithril_stake_distribution.certificate_hash
                )
            })?;

        let message = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&mithril_stake_distribution)
            .with_context(|| {
                "Can not compute the message for the given Mithril stake distribution"
            })?;

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
