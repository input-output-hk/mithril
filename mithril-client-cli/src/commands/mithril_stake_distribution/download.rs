use anyhow::Context;
use clap::Parser;
use std::sync::Arc;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::utils::{self, IndicatifFeedbackReceiver, ProgressOutputType, ProgressPrinter};
use crate::{
    CommandContext,
    commands::{SharedArgs, client_builder},
    configuration::{ConfigError, ConfigSource},
    utils::ExpanderUtils,
};
use mithril_client::MessageBuilder;
use mithril_client::MithrilResult;

/// Download and verify a Mithril stake distribution information. If the
/// verification fails, the file is not persisted.
#[derive(Parser, Debug, Clone)]
pub struct MithrilStakeDistributionDownloadCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Hash of the Mithril stake distribution artifact, or `latest` for the latest artifact.
    artifact_hash: String,

    /// Directory where the Mithril stake distribution will be downloaded.
    ///
    /// By default, a subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis verification key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl MithrilStakeDistributionDownloadCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?.add_source(self)?;
        let download_dir = params.get_or("download_dir", ".");
        let download_dir = Path::new(&download_dir);
        let logger = context.logger();

        let progress_output_type = if self.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 4);
        let client = client_builder(&params)?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
                logger.clone(),
            )))
            .with_logger(logger.clone())
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
        progress_printer.report_step(
            1,
            &format!(
                "Fetching Mithril stake distribution '{}' …",
                self.artifact_hash
            ),
        )?;
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

        progress_printer.report_step(
            2,
            "Fetching the certificate and verifying the certificate chain…",
        )?;
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

        progress_printer.report_step(
            3,
            "Verify that the Mithril stake distribution is signed in the associated certificate",
        )?;
        let message = MessageBuilder::new()
            .compute_mithril_stake_distribution_message(&certificate, &mithril_stake_distribution)
            .with_context(
                || "Can not compute the message for the given Mithril stake distribution",
            )?;

        if !certificate.match_message(&message) {
            return Err(anyhow::anyhow!(
                "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
                certificate.signed_message,
                message.compute_hash()
            ));
        }

        progress_printer.report_step(4, "Writing fetched Mithril stake distribution to a file")?;
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
                    "Can not serialize stake distribution artifact '{mithril_stake_distribution:?}'"
                )
            })?,
        )?;

        if self.is_json_output_enabled() {
            println!(
                r#"{{"mithril_stake_distribution_hash": "{}", "filepath": "{}"}}"#,
                mithril_stake_distribution.hash,
                filepath.display()
            );
        } else {
            println!(
                "Mithril stake distribution '{}' has been verified and saved as '{}'.",
                mithril_stake_distribution.hash,
                filepath.display()
            );
        }

        Ok(())
    }
}

impl ConfigSource for MithrilStakeDistributionDownloadCommand {
    fn collect(&self) -> Result<HashMap<String, String>, ConfigError> {
        let mut map = HashMap::new();

        if let Some(download_dir) = self.download_dir.clone() {
            let param = "download_dir".to_string();
            map.insert(
                param.clone(),
                utils::path_to_string(&download_dir)
                    .map_err(|e| ConfigError::Conversion(param, e))?,
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
