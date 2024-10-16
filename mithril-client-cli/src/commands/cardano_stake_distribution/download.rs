use anyhow::{anyhow, Context};
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use std::sync::Arc;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::utils::{ExpanderUtils, IndicatifFeedbackReceiver, ProgressOutputType, ProgressPrinter};
use crate::{
    commands::{client_builder, SharedArgs},
    configuration::{ConfigError, ConfigParameters, ConfigSource},
};
use mithril_client::common::Epoch;
use mithril_client::Client;
use mithril_client::{CardanoStakeDistribution, MessageBuilder, MithrilResult};

/// Download and verify a Cardano stake distribution information.
#[derive(Parser, Debug, Clone)]
pub struct CardanoStakeDistributionDownloadCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Epoch or hash of the Cardano stake distribution artifact.
    ///
    /// The epoch represents the epoch at the end of which the Cardano stake distribution is computed by the Cardano node.
    ///
    /// If `latest` is specified as unique_identifier, the command will return the latest Cardano stake distribution.
    unique_identifier: String,

    /// Directory where the Cardano stake distribution will be downloaded.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis Verification Key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl CardanoStakeDistributionDownloadCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Main command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        let config = config_builder.build()?;
        let params = ConfigParameters::new(config.try_deserialize::<HashMap<String, String>>()?)
            .add_source(self)?;
        let download_dir = params.get_or("download_dir", ".");
        let download_dir = Path::new(&download_dir);

        let progress_output_type = if self.is_json_output_enabled() {
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

        progress_printer.report_step(
            1,
            &format!(
                "Fetching Cardano stake distribution for identifier: '{}' …",
                self.unique_identifier
            ),
        )?;
        let cardano_stake_distribution =
            Self::fetch_cardano_stake_distribution_from_unique_identifier(
                &client,
                &self.unique_identifier,
            )
            .await
            .with_context(|| {
                format!(
                    "Can not fetch Cardano stake distribution from unique identifier: '{}'",
                    &self.unique_identifier
                )
            })?;

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

        if self.is_json_output_enabled() {
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

    fn is_sha256_hash(identifier: &str) -> bool {
        identifier.len() == 64 && identifier.chars().all(|c| c.is_ascii_hexdigit())
    }

    // The unique identifier can be either a SHA256 hash, an epoch,  or 'latest'.
    async fn fetch_cardano_stake_distribution_from_unique_identifier(
        client: &Client,
        unique_identifier: &str,
    ) -> MithrilResult<CardanoStakeDistribution> {
        let cardano_stake_distribution = if Self::is_sha256_hash(unique_identifier) {
            client
                .cardano_stake_distribution()
                .get(unique_identifier)
                .await
                .with_context(|| {
                    format!(
                        "Can not download and verify the artifact for hash: '{}'",
                        unique_identifier
                    )
                })?
                .ok_or(anyhow!(
                    "No Cardano stake distribution could be found for hash: '{}'",
                    unique_identifier
                ))
        } else {
            let epoch = {
                let get_list_of_artifact_epochs = || async {
                    let cardano_stake_distributions = client.cardano_stake_distribution().list().await.with_context(|| {
                        "Can not get the list of artifacts while retrieving the latest Cardano stake distribution epoch"
                    })?;

                    Ok(cardano_stake_distributions
                        .iter()
                        .map(|csd| csd.epoch.to_string())
                        .collect::<Vec<String>>())
                };

                let epoch = ExpanderUtils::expand_eventual_id_alias(
                    unique_identifier,
                    get_list_of_artifact_epochs(),
                )
                .await?;

                Epoch(
                    epoch.parse().with_context(|| {
                        format!("Can not convert: '{}' into a valid Epoch", epoch)
                    })?,
                )
            };

            client
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
                    "No Cardano stake distribution could be found for epoch: '{}'",
                    epoch
                ))
        };

        cardano_stake_distribution
    }
}

impl ConfigSource for CardanoStakeDistributionDownloadCommand {
    fn collect(&self) -> Result<HashMap<String, String>, ConfigError> {
        let mut map = HashMap::new();

        if let Some(download_dir) = self.download_dir.clone() {
            map.insert(
                "download_dir".to_string(),
                download_dir
                    .to_str()
                    .ok_or_else(|| {
                        ConfigError::Conversion(format!(
                            "Could not read download directory: '{}'.",
                            download_dir.display()
                        ))
                    })?
                    .to_string(),
            );
        }

        if let Some(genesis_verification_key) = self.genesis_verification_key.clone() {
            map.insert(
                "genesis_verification_key".to_string(),
                genesis_verification_key,
            );
        }

        Ok(map)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_sha_256_returns_false_with_len_different_than_64_and_hex_digit() {
        let len_65_hex_digit = "65aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        assert!(!CardanoStakeDistributionDownloadCommand::is_sha256_hash(
            len_65_hex_digit
        ));

        let len_63_hex_digit = "63aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        assert!(!CardanoStakeDistributionDownloadCommand::is_sha256_hash(
            len_63_hex_digit
        ));
    }

    #[test]
    fn is_sha_256_returns_false_with_len_equal_to_64_and_not_hex_digit() {
        let len_64_not_hex_digit =
            "64zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz";
        assert!(!CardanoStakeDistributionDownloadCommand::is_sha256_hash(
            len_64_not_hex_digit
        ));
    }

    #[test]
    fn is_sha_256_returns_true_with_len_equal_to_64_and_hex_digit() {
        let len_64_hex_digit = "64aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
        assert!(CardanoStakeDistributionDownloadCommand::is_sha256_hash(
            len_64_hex_digit
        ));
    }
}
