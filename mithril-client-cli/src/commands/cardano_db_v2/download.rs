use std::{
    collections::HashMap,
    fs::File,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use chrono::Utc;
use clap::Parser;
use slog::{debug, warn, Logger};

use mithril_client::{
    cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange},
    common::{ImmutableFileNumber, MKProof, ProtocolMessage},
    CardanoDatabaseSnapshot, Client, MessageBuilder, MithrilCertificate, MithrilResult,
};

use crate::{
    commands::{client_builder, SharedArgs},
    configuration::{ConfigError, ConfigSource},
    utils::{
        self, CardanoDbDownloadChecker, CardanoDbUtils, ExpanderUtils, IndicatifFeedbackReceiver,
        ProgressOutputType, ProgressPrinter,
    },
    CommandContext,
};

struct RestorationOptions {
    db_dir: PathBuf,
    immutable_file_range: ImmutableFileRange,
    download_unpack_options: DownloadUnpackOptions,
}

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

    /// The first immutable file number to download.
    ///
    /// If not set, the download process will start from the first immutable file.
    #[clap(long)]
    start: Option<ImmutableFileNumber>,

    /// The last immutable file number to download.
    ///
    /// If not set, the download will continue until the last certified immutable file.
    #[clap(long)]
    end: Option<ImmutableFileNumber>,

    /// Include ancillary files in the download.
    ///
    /// By default, only immutable files are downloaded.
    /// The ledger files, volatile files, and the latest unfinished immutable files are not taken into account.
    #[clap(long)]
    include_ancillary: bool,

    /// Allow existing files in the download directory to be overridden.
    #[clap(long)]
    allow_override: bool,
}

impl CardanoDbV2DownloadCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    fn immutable_file_range(
        start: Option<ImmutableFileNumber>,
        end: Option<ImmutableFileNumber>,
    ) -> ImmutableFileRange {
        match (start, end) {
            (None, None) => ImmutableFileRange::Full,
            (Some(start), None) => ImmutableFileRange::From(start),
            (Some(start), Some(end)) => ImmutableFileRange::Range(start, end),
            (None, Some(end)) => ImmutableFileRange::UpTo(end),
        }
    }

    /// Command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let params = context.config_parameters()?.add_source(self)?;
        let download_dir: &String = &params.require("download_dir")?;
        let restoration_options = RestorationOptions {
            db_dir: Path::new(download_dir).join("db_v2"),
            immutable_file_range: Self::immutable_file_range(self.start, self.end),
            download_unpack_options: DownloadUnpackOptions {
                allow_override: self.allow_override,
                include_ancillary: self.include_ancillary,
                ..DownloadUnpackOptions::default()
            },
        };
        let logger = context.logger();

        let progress_output_type = if self.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 6);
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

        Self::check_local_disk_info(
            1,
            &progress_printer,
            &restoration_options.db_dir,
            &cardano_db_message,
            self.allow_override,
        )?;

        let certificate = Self::fetch_certificate_and_verifying_chain(
            2,
            &progress_printer,
            &client,
            &cardano_db_message.certificate_hash,
        )
        .await?;

        Self::download_and_unpack_cardano_database_snapshot(
            logger,
            3,
            &progress_printer,
            &client,
            &cardano_db_message,
            &restoration_options,
        )
        .await
        .with_context(|| {
            format!(
                "Can not download and unpack cardano db snapshot for hash: '{}'",
                self.hash
            )
        })?;

        let merkle_proof = Self::compute_verify_merkle_proof(
            4,
            &progress_printer,
            &client,
            &certificate,
            &cardano_db_message,
            &restoration_options.immutable_file_range,
            &restoration_options.db_dir,
        )
        .await?;

        let message = Self::compute_cardano_db_snapshot_message(
            5,
            &progress_printer,
            &certificate,
            &merkle_proof,
        )
        .await?;

        Self::verify_cardano_db_snapshot_signature(
            logger,
            6,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db_message,
            &restoration_options.db_dir,
        )
        .await?;

        Self::log_download_information(
            &restoration_options.db_dir,
            &cardano_db_message,
            self.is_json_output_enabled(),
        )?;

        Ok(())
    }

    fn check_local_disk_info(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        db_dir: &Path,
        cardano_db: &CardanoDatabaseSnapshot,
        allow_override: bool,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Checking local disk info…")?;

        CardanoDbDownloadChecker::ensure_dir_exist(db_dir)?;
        if let Err(e) = CardanoDbDownloadChecker::check_prerequisites_for_uncompressed_data(
            db_dir,
            cardano_db.total_db_size_uncompressed,
            allow_override,
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

    async fn download_and_unpack_cardano_database_snapshot(
        logger: &Logger,
        step_number: u16,
        progress_printer: &ProgressPrinter,
        client: &Client,
        cardano_database_snapshot: &CardanoDatabaseSnapshot,
        restoration_options: &RestorationOptions,
    ) -> MithrilResult<()> {
        progress_printer.report_step(
            step_number,
            "Downloading and unpacking the cardano db snapshot",
        )?;
        client
            .cardano_database()
            .download_unpack(
                cardano_database_snapshot,
                &restoration_options.immutable_file_range,
                &restoration_options.db_dir,
                restoration_options.download_unpack_options,
            )
            .await?;

        // The cardano db snapshot download does not fail if the statistic call fails.
        // It would be nice to implement tests to verify the behavior of `add_statistics`
        let full_restoration = restoration_options.immutable_file_range == ImmutableFileRange::Full;
        let include_ancillary = restoration_options
            .download_unpack_options
            .include_ancillary;
        let number_of_immutable_files_restored = restoration_options
            .immutable_file_range
            .length(cardano_database_snapshot.beacon.immutable_file_number);
        if let Err(e) = client
            .cardano_database()
            .add_statistics(
                full_restoration,
                include_ancillary,
                number_of_immutable_files_restored,
            )
            .await
        {
            warn!(
                logger, "Could not increment cardano db snapshot download statistics";
                "error" => ?e
            );
        }

        // Append 'clean' file to speedup node bootstrap
        if let Err(error) = File::create(restoration_options.db_dir.join("clean")) {
            warn!(
                logger, "Could not create clean shutdown marker file in directory '{}'", restoration_options.db_dir.display();
                "error" => error.to_string()
            );
        };

        Ok(())
    }

    async fn compute_verify_merkle_proof(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        client: &Client,
        certificate: &MithrilCertificate,
        cardano_database_snapshot: &CardanoDatabaseSnapshot,
        immutable_file_range: &ImmutableFileRange,
        unpacked_dir: &Path,
    ) -> MithrilResult<MKProof> {
        progress_printer.report_step(step_number, "Computing and verifying the Merkle proof…")?;
        let merkle_proof = client
            .cardano_database()
            .compute_merkle_proof(
                certificate,
                cardano_database_snapshot,
                immutable_file_range,
                unpacked_dir,
            )
            .await?;

        merkle_proof
            .verify()
            .with_context(|| "Merkle proof verification failed")?;

        Ok(merkle_proof)
    }

    async fn compute_cardano_db_snapshot_message(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        certificate: &MithrilCertificate,
        merkle_proof: &MKProof,
    ) -> MithrilResult<ProtocolMessage> {
        progress_printer.report_step(step_number, "Computing the cardano db snapshot message")?;
        let message = CardanoDbUtils::wait_spinner(
            progress_printer,
            MessageBuilder::new().compute_cardano_database_message(certificate, merkle_proof),
        )
        .await
        .with_context(|| "Can not compute the cardano db snapshot message")?;

        Ok(message)
    }

    async fn verify_cardano_db_snapshot_signature(
        logger: &Logger,
        step_number: u16,
        progress_printer: &ProgressPrinter,
        certificate: &MithrilCertificate,
        message: &ProtocolMessage,
        cardano_db_snapshot: &CardanoDatabaseSnapshot,
        db_dir: &Path,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Verifying the cardano db signature…")?;
        if !certificate.match_message(message) {
            debug!(
                logger,
                "Merkle root verification failed, removing unpacked files & directory."
            );

            if let Err(error) = std::fs::remove_dir_all(db_dir) {
                warn!(
                    logger, "Error while removing unpacked files & directory";
                    "error" => error.to_string()
                );
            }

            return Err(anyhow!(
                "Certificate verification failed (cardano db snapshot hash = '{}').",
                cardano_db_snapshot.hash.clone()
            ));
        }

        Ok(())
    }

    fn log_download_information(
        db_dir: &Path,
        cardano_db_snapshot: &CardanoDatabaseSnapshot,
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
            let cardano_node_version = &cardano_db_snapshot.cardano_node_version;
            println!(
                r###"Cardano database snapshot '{}' archives have been successfully unpacked. Immutable files have been successfully checked against Mithril multi-signature contained in the certificate.
                    
    Files in the directory '{}' can be used to run a Cardano node with version >= {cardano_node_version}.

    If you are using Cardano Docker image, you can restore a Cardano Node with:
    
    docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} ghcr.io/intersectmbo/cardano-node:{cardano_node_version}
    
    "###,
                cardano_db_snapshot.hash,
                db_dir.display(),
                canonicalized_filepath.display(),
                cardano_db_snapshot.network,
            );
        }

        Ok(())
    }
}

impl ConfigSource for CardanoDbV2DownloadCommand {
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

#[cfg(test)]
mod tests {
    use mithril_client::{
        common::{CardanoDbBeacon, ProtocolMessagePartKey, SignedEntityType},
        MithrilCertificateMetadata,
    };
    use mithril_common::test_utils::TempDir;

    use super::*;

    fn dummy_certificate() -> MithrilCertificate {
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            CardanoDatabaseSnapshot::dummy().hash.to_string(),
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
            signed_entity_type: SignedEntityType::CardanoDatabase(beacon),
            metadata: MithrilCertificateMetadata::dummy(),
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: String::new(),
            multi_signature: String::new(),
            genesis_signature: String::new(),
        }
    }

    #[tokio::test]
    async fn verify_cardano_db_snapshot_signature_should_remove_db_dir_if_messages_mismatch() {
        let progress_printer = ProgressPrinter::new(ProgressOutputType::Tty, 1);
        let certificate = dummy_certificate();
        let mut message = ProtocolMessage::new();
        message.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            "merkle-root-123456".to_string(),
        );
        message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "avk-123456".to_string(),
        );
        let cardano_db = CardanoDatabaseSnapshot::dummy();
        let db_dir = TempDir::create(
            "client-cli",
            "verify_cardano_db_snapshot_signature_should_remove_db_dir_if_messages_mismatch",
        );

        let result = CardanoDbV2DownloadCommand::verify_cardano_db_snapshot_signature(
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

    #[test]
    fn immutable_file_range_without_start_without_end_returns_variant_full() {
        let range = CardanoDbV2DownloadCommand::immutable_file_range(None, None);

        assert_eq!(range, ImmutableFileRange::Full);
    }

    #[test]
    fn immutable_file_range_with_start_without_end_returns_variant_from() {
        let start = Some(12);

        let range = CardanoDbV2DownloadCommand::immutable_file_range(start, None);

        assert_eq!(range, ImmutableFileRange::From(12));
    }

    #[test]
    fn immutable_file_range_with_start_with_end_returns_variant_range() {
        let start = Some(12);
        let end = Some(345);

        let range = CardanoDbV2DownloadCommand::immutable_file_range(start, end);

        assert_eq!(range, ImmutableFileRange::Range(12, 345));
    }

    #[test]
    fn immutable_file_range_without_start_with_end_returns_variant_up_to() {
        let end = Some(345);

        let range = CardanoDbV2DownloadCommand::immutable_file_range(None, end);

        assert_eq!(range, ImmutableFileRange::UpTo(345));
    }
}
