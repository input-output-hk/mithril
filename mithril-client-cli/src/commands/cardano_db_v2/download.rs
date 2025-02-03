use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Context;
use clap::Parser;

use mithril_client::{CardanoDatabaseSnapshot, Client, MithrilCertificate, MithrilResult};

use crate::{
    commands::{client_builder, SharedArgs},
    configuration::{ConfigError, ConfigSource},
    utils::{
        CardanoDbDownloadChecker, CardanoDbUtils, ExpanderUtils, IndicatifFeedbackReceiver,
        ProgressOutputType, ProgressPrinter,
    },
    CommandContext,
};

/// Clap command to download a Cardano db and verify its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbV2DownloadCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Hash of the cardano db to download. Use the `list` command to get that information.
    ///
    /// If `latest` is specified as hash, the command will return the latest cardano db.
    hash: String,

    /// Directory where the immutable and ancillary files will be downloaded. By default, a
    /// subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis Verification Key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl CardanoDbV2DownloadCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?.add_source(self)?;
        let download_dir: &String = &params.require("download_dir")?;
        let db_dir = Path::new(download_dir).join("db_v2");
        let logger = context.logger();

        let progress_output_type = if self.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 2);
        let client = client_builder(&params)?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
                logger.clone(),
            )))
            .with_logger(logger.clone())
            .build()?;

        let get_list_of_artifact_ids = || async {
            let cardano_db_snapshots =
                client.cardano_database().list().await.with_context(|| {
                    "Can not get the list of artifacts while retrieving the latest cardano db hash"
                })?;

            Ok(cardano_db_snapshots
                .iter()
                .map(|cardano_db| cardano_db.hash.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.hash, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .with_context(|| format!("Can not get the cardano db for hash: '{}'", self.hash))?;

        Self::check_local_disk_info(1, &progress_printer, &db_dir, &cardano_db_message)?;

        let _certificate = Self::fetch_certificate_and_verifying_chain(
            2,
            &progress_printer,
            &client,
            &cardano_db_message.certificate_hash,
        )
        .await?;

        Ok(())
    }

    fn check_local_disk_info(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        db_dir: &Path,
        cardano_db: &CardanoDatabaseSnapshot,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Checking local disk info…")?;

        CardanoDbDownloadChecker::ensure_dir_exist(db_dir)?;
        if let Err(e) = CardanoDbDownloadChecker::check_prerequisites(
            db_dir,
            cardano_db.total_db_size_uncompressed,
            cardano_db.compression_algorithm,
        ) {
            progress_printer
                .report_step(step_number, &CardanoDbUtils::check_disk_space_error(e)?)?;
        }

        Ok(())
    }

    async fn fetch_certificate_and_verifying_chain(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        client: &Client,
        certificate_hash: &str,
    ) -> MithrilResult<MithrilCertificate> {
        progress_printer.report_step(
            step_number,
            "Fetching the certificate and verifying the certificate chain…",
        )?;
        let certificate = client
            .certificate()
            .verify_chain(certificate_hash)
            .await
            .with_context(|| {
                format!(
                    "Can not verify the certificate chain from certificate_hash: '{}'",
                    certificate_hash
                )
            })?;

        Ok(certificate)
    }
}

impl ConfigSource for CardanoDbV2DownloadCommand {
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
