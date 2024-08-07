use anyhow::{anyhow, Context};
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use std::sync::Arc;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::utils::{ExpanderUtils, IndicatifFeedbackReceiver, ProgressOutputType, ProgressPrinter};
use crate::{commands::client_builder, configuration::ConfigParameters};
use mithril_client::common::Epoch;
use mithril_client::MessageBuilder;
use mithril_client::MithrilResult;

/// Download and verify a Cardano stake distribution information.
#[derive(Parser, Debug, Clone)]
pub struct CardanoStakeDistributionDownloadCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Epoch of the Cardano stake distribution artifact.
    /// It represents the epoch at the end of which the Cardano stake distribution is computed by the Cardano node
    ///
    /// If `latest` is specified as epoch, the command will return the latest Cardano stake distribution.
    epoch: String,

    /// Directory where the Cardano stake distribution will be downloaded.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis Verification Key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl CardanoStakeDistributionDownloadCommand {
    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        let config = config_builder
            .set_default("download_dir", ".")?
            .add_source(self.clone())
            .build()?;
        let params = ConfigParameters::new(config.try_deserialize::<HashMap<String, String>>()?);
        let download_dir = &params.require("download_dir")?;
        let download_dir = Path::new(download_dir);

        let progress_output_type = if self.json {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 4);
        let client = client_builder(&params)?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
            )))
            .build()?;

        let get_list_of_artifact_epochs = || async {
            let cardano_stake_distributions = client.cardano_stake_distribution().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest Cardano stake distribution epoch"
            })?;

            Ok(cardano_stake_distributions
                .iter()
                .map(|csd| csd.epoch.to_string())
                .collect::<Vec<String>>())
        };

        let epoch =
            ExpanderUtils::expand_eventual_id_alias(&self.epoch, get_list_of_artifact_epochs())
                .await?;
        let epoch = Epoch(
            epoch
                .parse()
                .with_context(|| format!("Can not convert: '{}' into a valid Epoch", epoch))?,
        );

        progress_printer.report_step(
            1,
            &format!(
                "Fetching Cardano stake distribution for epoch: '{}' …",
                epoch
            ),
        )?;
        let cardano_stake_distribution = client
            .cardano_stake_distribution()
            .get_by_epoch(epoch)
            .await
            .with_context(|| {
                format!(
                    "Can not download and verify the artifact for epoch: '{}'",
                    epoch
                )
            })?
            .ok_or(anyhow!(
                "Cardano stake distribution for epoch '{}' not found",
                epoch
            ))?;

        progress_printer.report_step(
            2,
            "Fetching the certificate and verifying the certificate chain…",
        )?;
        let certificate = client
            .certificate()
            .verify_chain(&cardano_stake_distribution.certificate_hash)
            .await
            .with_context(|| {
                format!(
                    "Can not verify the certificate chain from certificate_hash: '{}'",
                    &cardano_stake_distribution.certificate_hash
                )
            })?;

        progress_printer.report_step(
            3,
            "Verify that the Cardano stake distribution is signed in the associated certificate",
        )?;
        let message = MessageBuilder::new()
            .compute_cardano_stake_distribution_message(&certificate, &cardano_stake_distribution)
            .with_context(|| {
                "Can not compute the message for the given Cardano stake distribution"
            })?;

        if !certificate.match_message(&message) {
            return Err(anyhow!(
                    "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
                    certificate.signed_message,
                    message.compute_hash()
                ));
        }

        progress_printer.report_step(4, "Writing fetched Cardano stake distribution to a file")?;
        if !download_dir.is_dir() {
            std::fs::create_dir_all(download_dir)?;
        }
        let filepath = PathBuf::new().join(download_dir).join(format!(
            "cardano_stake_distribution-{}.json",
            cardano_stake_distribution.epoch
        ));
        std::fs::write(
            &filepath,
            serde_json::to_string(&cardano_stake_distribution).with_context(|| {
                format!(
                    "Can not serialize Cardano stake distribution artifact '{:?}'",
                    cardano_stake_distribution
                )
            })?,
        )?;

        if self.json {
            println!(
                r#"{{"cardano_stake_distribution_epoch": "{}", "filepath": "{}"}}"#,
                cardano_stake_distribution.epoch,
                filepath.display()
            );
        } else {
            println!(
                "Cardano stake distribution for epoch '{}' has been verified and saved as '{}'.",
                cardano_stake_distribution.epoch,
                filepath.display()
            );
        }

        Ok(())
    }
}

impl Source for CardanoStakeDistributionDownloadCommand {
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
