use anyhow::{anyhow, Context};
use chrono::Utc;
use clap::Parser;
use slog::{debug, warn, Logger};
use std::{
    collections::HashMap,
    fs::File,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    commands::{client_builder, SharedArgs},
    configuration::{ConfigError, ConfigParameters, ConfigSource},
    utils::{
        self, AncillaryLogMessage, CardanoDbDownloadChecker, CardanoDbUtils, ExpanderUtils,
        IndicatifFeedbackReceiver, ProgressOutputType, ProgressPrinter,
    },
    CommandContext,
};
use mithril_client::{
    common::ProtocolMessage, snapshot_client::SnapshotClient, Client, MessageBuilder,
    MithrilCertificate, MithrilResult, Snapshot,
};

/// Clap command to download a Cardano db and verify its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbDownloadCommand {
    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Digest of the Cardano db snapshot to download  or `latest` for the latest artifact
    ///
    /// Use the `list` command to get that information.
    digest: String,

    /// Directory where the immutable and ancillary files will be downloaded.
    ///
    /// By default, a subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis verification key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,

    /// Include ancillary files in the download, if set the `ancillary_verification_key` is required
    /// in order to verify the ancillary files.
    ///
    /// By default, only finalized immutable files are downloaded.
    /// The last ledger state snapshot and the last immutable file (the ancillary files) can be
    /// downloaded with this option.
    #[clap(long)]
    include_ancillary: bool,

    /// Ancillary verification key to verify the ancillary files.
    #[clap(long, env = "ANCILLARY_VERIFICATION_KEY")]
    ancillary_verification_key: Option<String>,
}

impl CardanoDbDownloadCommand {
    /// Command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?.add_source(self)?;
        let prepared_command = self.prepare(&params)?;

        prepared_command.execute(context.logger(), params).await
    }

    fn prepare(&self, params: &ConfigParameters) -> MithrilResult<PreparedCardanoDbDownload> {
        let ancillary_verification_key = if self.include_ancillary {
            AncillaryLogMessage::warn_ancillary_not_signed_by_mithril();
            Some(params.require("ancillary_verification_key")?)
        } else {
            AncillaryLogMessage::warn_fast_bootstrap_not_available();
            None
        };

        Ok(PreparedCardanoDbDownload {
            shared_args: self.shared_args.clone(),
            digest: self.digest.clone(),
            download_dir: params.require("download_dir")?,
            include_ancillary: self.include_ancillary,
            ancillary_verification_key,
        })
    }
}

#[derive(Debug, Clone)]
struct PreparedCardanoDbDownload {
    shared_args: SharedArgs,
    digest: String,
    download_dir: String,
    include_ancillary: bool,
    ancillary_verification_key: Option<String>,
}

impl PreparedCardanoDbDownload {
    fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Command execution
    pub async fn execute(&self, logger: &Logger, params: ConfigParameters) -> MithrilResult<()> {
        let db_dir = Path::new(&self.download_dir).join("db");

        let progress_output_type = if self.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 5);
        let client = client_builder(&params)?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
                logger.clone(),
            )))
            .set_ancillary_verification_key(self.ancillary_verification_key.clone())
            .with_logger(logger.clone())
            .build()?;

        let get_list_of_artifact_ids = || async {
            let cardano_dbs = client.cardano_database().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest cardano db digest"
            })?;

            Ok(cardano_dbs
                .iter()
                .map(|cardano_db| cardano_db.digest.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.digest, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .with_context(|| format!("Can not get the cardano db for digest: '{}'", self.digest))?;

        Self::check_local_disk_info(1, &progress_printer, &db_dir, &cardano_db_message)?;

        let certificate = Self::fetch_certificate_and_verifying_chain(
            2,
            &progress_printer,
            &client,
            &cardano_db_message.certificate_hash,
        )
        .await?;

        Self::download_and_unpack_cardano_db(
            logger,
            3,
            &progress_printer,
            client.cardano_database(),
            &cardano_db_message,
            self.include_ancillary,
            &db_dir,
        )
        .await
        .with_context(|| {
            format!(
                "Can not get download and unpack cardano db for digest: '{}'",
                self.digest
            )
        })?;

        let message =
            Self::compute_cardano_db_message(4, &progress_printer, &certificate, &db_dir).await?;

        Self::verify_cardano_db_signature(
            logger,
            5,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db_message,
            &db_dir,
        )
        .await?;

        Self::log_download_information(
            &db_dir,
            &cardano_db_message,
            self.is_json_output_enabled(),
        )?;

        Ok(())
    }

    fn check_local_disk_info(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        db_dir: &Path,
        cardano_db: &Snapshot,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Checking local disk info…")?;

        CardanoDbDownloadChecker::ensure_dir_exist(db_dir)?;
        if let Err(e) = CardanoDbDownloadChecker::check_prerequisites_for_archive(
            db_dir,
            cardano_db.compute_total_size(),
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
                    "Can not verify the certificate chain from certificate_hash: '{certificate_hash}'"
                )
            })?;

        Ok(certificate)
    }

    async fn download_and_unpack_cardano_db(
        logger: &Logger,
        step_number: u16,
        progress_printer: &ProgressPrinter,
        snapshot_client: Arc<SnapshotClient>,
        cardano_db: &Snapshot,
        include_ancillary: bool,
        db_dir: &Path,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Downloading and unpacking the cardano db")?;

        if include_ancillary {
            snapshot_client
                .download_unpack_full(cardano_db, db_dir)
                .await?;
        } else {
            snapshot_client.download_unpack(cardano_db, db_dir).await?;
        }

        // The Cardano db download does not fail if the statistic call fails.
        // It would be nice to implement tests to verify the behavior of `add_statistics`
        if let Err(e) = snapshot_client.add_statistics(cardano_db).await {
            warn!(
                logger, "Could not increment cardano db download statistics";
                "error" => ?e
            );
        }

        // Append 'clean' file to speedup node bootstrap
        if let Err(error) = File::create(db_dir.join("clean")) {
            warn!(
                logger, "Could not create clean shutdown marker file in directory '{}'", db_dir.display();
                "error" => error.to_string()
            );
        };

        Ok(())
    }

    async fn compute_cardano_db_message(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        certificate: &MithrilCertificate,
        db_dir: &Path,
    ) -> MithrilResult<ProtocolMessage> {
        progress_printer.report_step(step_number, "Computing the cardano db message")?;
        let message = CardanoDbUtils::wait_spinner(
            progress_printer,
            MessageBuilder::new().compute_snapshot_message(certificate, db_dir),
        )
        .await
        .with_context(|| {
            format!("Can not compute the cardano db message from the directory: '{db_dir:?}'")
        })?;

        Ok(message)
    }

    async fn verify_cardano_db_signature(
        logger: &Logger,
        step_number: u16,
        progress_printer: &ProgressPrinter,
        certificate: &MithrilCertificate,
        message: &ProtocolMessage,
        cardano_db: &Snapshot,
        db_dir: &Path,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Verifying the cardano db signature…")?;
        if !certificate.match_message(message) {
            debug!(
                logger,
                "Digest verification failed, removing unpacked files & directory."
            );

            if let Err(error) = std::fs::remove_dir_all(db_dir) {
                warn!(
                    logger, "Error while removing unpacked files & directory";
                    "error" => error.to_string()
                );
            }

            return Err(anyhow!(
                "Certificate verification failed (cardano db digest = '{}').",
                cardano_db.digest.clone()
            ));
        }

        Ok(())
    }

    fn log_download_information(
        db_dir: &Path,
        cardano_db: &Snapshot,
        json_output: bool,
    ) -> MithrilResult<()> {
        let canonicalized_filepath = &db_dir.canonicalize().with_context(|| {
            format!(
                "Could not get canonicalized filepath of '{}'",
                db_dir.display()
            )
        })?;

        if json_output {
            println!(
                r#"{{"timestamp": "{}", "db_directory": "{}"}}"#,
                Utc::now().to_rfc3339(),
                canonicalized_filepath.display()
            );
        } else {
            let cardano_node_version = &cardano_db.cardano_node_version;
            println!(
                r###"Cardano db '{}' has been unpacked and successfully checked against Mithril multi-signature contained in the certificate.
                    
    Files in the directory '{}' can be used to run a Cardano node with version >= {cardano_node_version}.
    
    If you are using Cardano Docker image, you can restore a Cardano Node with:
    
    docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} ghcr.io/intersectmbo/cardano-node:{cardano_node_version}
    
    "###,
                cardano_db.digest,
                db_dir.display(),
                canonicalized_filepath.display(),
                cardano_db.network,
            );
        }

        Ok(())
    }
}

impl ConfigSource for CardanoDbDownloadCommand {
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

        if let Some(ancillary_verification_key) = self.ancillary_verification_key.clone() {
            map.insert(
                "ancillary_verification_key".to_string(),
                ancillary_verification_key,
            );
        }

        Ok(map)
    }
}
#[cfg(test)]
mod tests {
    use config::ConfigBuilder;
    use mithril_client::{
        common::{CardanoDbBeacon, ProtocolMessagePartKey, SignedEntityType},
        MithrilCertificateMetadata,
    };
    use mithril_common::test_utils::TempDir;

    use super::*;

    fn dummy_certificate() -> MithrilCertificate {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            Snapshot::dummy().digest.to_string(),
        );
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "whatever".to_string(),
        );
        let beacon = CardanoDbBeacon::new(10, 100);

        MithrilCertificate {
            hash: "hash".to_string(),
            previous_hash: "previous_hash".to_string(),
            epoch: beacon.epoch,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(beacon),
            metadata: MithrilCertificateMetadata::dummy(),
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: String::new(),
            multi_signature: String::new(),
            genesis_signature: String::new(),
        }
    }

    fn dummy_command() -> CardanoDbDownloadCommand {
        CardanoDbDownloadCommand {
            shared_args: SharedArgs { json: false },
            digest: "whatever_digest".to_string(),
            download_dir: Some(std::path::PathBuf::from("whatever_dir")),
            genesis_verification_key: "whatever".to_string().into(),
            include_ancillary: true,
            ancillary_verification_key: "whatever".to_string().into(),
        }
    }

    #[tokio::test]
    async fn ancillary_verification_key_is_mandatory_when_include_ancillary_is_true() {
        let command = CardanoDbDownloadCommand {
            include_ancillary: true,
            ancillary_verification_key: None,
            ..dummy_command()
        };
        let command_context = CommandContext::new(
            ConfigBuilder::default(),
            false,
            Logger::root(slog::Discard, slog::o!()),
        );

        let result = command.execute(command_context).await;

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Parameter 'ancillary_verification_key' is mandatory."
        );
    }

    #[test]
    fn ancillary_verification_key_can_be_read_through_configuration_file() {
        let command = CardanoDbDownloadCommand {
            ancillary_verification_key: None,
            ..dummy_command()
        };
        let config = config::Config::builder()
            .set_default("ancillary_verification_key", "value from config")
            .expect("Failed to build config builder");
        let command_context =
            CommandContext::new(config, false, Logger::root(slog::Discard, slog::o!()));
        let config_parameters = command_context
            .config_parameters()
            .unwrap()
            .add_source(&command)
            .unwrap();

        let result = command.prepare(&config_parameters);

        assert!(result.is_ok());
    }

    #[test]
    fn db_download_dir_is_mandatory_to_execute_command() {
        let command = CardanoDbDownloadCommand {
            download_dir: None,
            ..dummy_command()
        };
        let command_context = CommandContext::new(
            ConfigBuilder::default(),
            false,
            Logger::root(slog::Discard, slog::o!()),
        );
        let config_parameters = command_context
            .config_parameters()
            .unwrap()
            .add_source(&command)
            .unwrap();

        let result = command.prepare(&config_parameters);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Parameter 'download_dir' is mandatory."
        );
    }

    #[tokio::test]
    async fn verify_cardano_db_signature_should_remove_db_dir_if_messages_mismatch() {
        let progress_printer = ProgressPrinter::new(ProgressOutputType::Tty, 1);
        let certificate = dummy_certificate();
        let mut message = ProtocolMessage::new();
        message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, "digest".to_string());
        message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "avk".to_string(),
        );
        let cardano_db = Snapshot::dummy();
        let db_dir = TempDir::create(
            "client-cli",
            "verify_cardano_db_signature_should_remove_db_dir_if_messages_mismatch",
        );

        let result = PreparedCardanoDbDownload::verify_cardano_db_signature(
            &Logger::root(slog::Discard, slog::o!()),
            1,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db,
            &db_dir,
        )
        .await;

        assert!(result.is_err());
        assert!(
            !db_dir.exists(),
            "The db directory should have been removed but it still exists"
        );
    }
}
