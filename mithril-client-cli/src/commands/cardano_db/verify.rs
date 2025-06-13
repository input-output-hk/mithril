use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{anyhow, Context};
use chrono::Utc;
use clap::Parser;
use mithril_client::{
    cardano_database_client::ImmutableFileRange,
    common::{ImmutableFileNumber, MKProof, ProtocolMessage},
    CardanoDatabaseSnapshot, Client, MessageBuilder, MithrilCertificate, MithrilResult,
};
use slog::{debug, warn, Logger};

use crate::{
    commands::{cardano_db::CardanoDbCommandsBackend, client_builder, SharedArgs},
    configuration::{ConfigError, ConfigParameters, ConfigSource},
    utils::{
        self, CardanoDbUtils, ExpanderUtils, IndicatifFeedbackReceiver, ProgressOutputType,
        ProgressPrinter,
    },
    CommandContext,
};

use super::download::shared_steps;

/// Clap command to verify a Cardano db and its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbVerifyCommand {
    #[arg(short, long, value_enum, default_value_t)]
    backend: CardanoDbCommandsBackend,

    #[clap(flatten)]
    shared_args: SharedArgs,

    /// Digest of the Cardano db snapshot to verify  or `latest` for the latest artifact
    ///
    /// Use the `list` command to get that information.
    digest: String,

    /// Directory from where the immutable will be verified.
    #[clap(long)]
    db_dir: Option<PathBuf>,

    /// Genesis verification key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl CardanoDbVerifyCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self.backend {
            CardanoDbCommandsBackend::V1 => Err(anyhow::anyhow!(
                "The \"verify\" subcommand is not available for the v1, use --backend v2 instead",
            )),
            CardanoDbCommandsBackend::V2 => {
                // *Important*: All parameters also available in the collected config source below MUST be
                // obtained using the `params` argument.
                // No need for `require` as validation of existence will be done by the `NewCardanoDbDownloadCommand` itself.
                let params = context.config_parameters()?.add_source(self)?;
                self.verify(context.logger(), params).await
            }
        }
    }

    async fn verify(&self, logger: &Logger, params: ConfigParameters) -> MithrilResult<()> {
        let db_dir = params.require("db_dir")?;
        let db_dir = Path::new(&db_dir);

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

        client
            .cardano_database_v2()
            .check_presence_of_immutables(db_dir)?;

        let get_list_of_artifact_ids = || async {
            let cardano_db_snapshots =
                client.cardano_database_v2().list().await.with_context(|| {
                    "Can not get the list of artifacts while retrieving the latest cardano db hash"
                })?;

            Ok(cardano_db_snapshots
                .iter()
                .map(|cardano_db| cardano_db.hash.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .cardano_database_v2()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.digest, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .with_context(|| format!("Can not get the cardano db for hash: '{}'", self.digest))?;

        let certificate = shared_steps::fetch_certificate_and_verifying_chain(
            1,
            &progress_printer,
            &client,
            &cardano_db_message.certificate_hash,
        )
        .await?;

        let immutable_file_range = immutable_file_range(None, None);

        let merkle_proof = compute_verify_merkle_proof(
            2,
            &progress_printer,
            &client,
            &certificate,
            &cardano_db_message,
            &immutable_file_range,
            db_dir,
        )
        .await?;

        let message =
            compute_cardano_db_snapshot_message(3, &progress_printer, &certificate, &merkle_proof)
                .await?;

        verify_cardano_db_snapshot_signature(
            logger,
            4,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db_message,
            db_dir,
        )
        .await?;

        Self::log_verified_information(
            db_dir,
            &cardano_db_message.hash,
            self.is_json_output_enabled(),
        )?;

        Ok(())
    }

    fn log_verified_information(
        db_dir: &Path,
        snapshot_hash: &str,
        json_output: bool,
    ) -> MithrilResult<()> {
        if json_output {
            let canonicalized_filepath = &db_dir.canonicalize().with_context(|| {
                format!(
                    "Could not get canonicalized filepath of '{}'",
                    db_dir.display()
                )
            })?;
            let json = serde_json::json!({
                "timestamp": Utc::now().to_rfc3339(),
                "verified_db_directory": canonicalized_filepath
            });
            println!("{}", json);
        } else {
            println!("Cardano database snapshot '{snapshot_hash}' archives have been successfully verified. Immutable files have been successfully checked against Mithril multi-signature contained in the certificate.");
        }
        Ok(())
    }
}

impl ConfigSource for CardanoDbVerifyCommand {
    fn collect(&self) -> Result<HashMap<String, String>, ConfigError> {
        let mut map = HashMap::new();

        if let Some(download_dir) = self.db_dir.clone() {
            let param = "db_dir".to_string();
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

/// Computes and verifies the Merkle proof for the given certificate and database snapshot.
pub async fn compute_verify_merkle_proof(
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
        .cardano_database_v2()
        .compute_merkle_proof(
            certificate,
            cardano_database_snapshot,
            immutable_file_range,
            Path::new(&unpacked_dir),
        )
        .await?;

    merkle_proof
        .verify()
        .with_context(|| "Merkle proof verification failed")?;

    Ok(merkle_proof)
}

/// Verifies the signature of the Cardano db snapshot against the certificate and message.
pub async fn verify_cardano_db_snapshot_signature(
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

/// Constructs an `ImmutableFileRange` based on the provided start and end file numbers.
pub fn immutable_file_range(
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

/// Computes the Cardano db snapshot message using the provided certificate and Merkle proof.
pub async fn compute_cardano_db_snapshot_message(
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

        let result = verify_cardano_db_snapshot_signature(
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
        let range = immutable_file_range(None, None);

        assert_eq!(range, ImmutableFileRange::Full);
    }

    #[test]
    fn immutable_file_range_with_start_without_end_returns_variant_from() {
        let start = Some(12);

        let range = immutable_file_range(start, None);

        assert_eq!(range, ImmutableFileRange::From(12));
    }

    #[test]
    fn immutable_file_range_with_start_with_end_returns_variant_range() {
        let start = Some(12);
        let end = Some(345);

        let range = immutable_file_range(start, end);

        assert_eq!(range, ImmutableFileRange::Range(12, 345));
    }

    #[test]
    fn immutable_file_range_without_start_with_end_returns_variant_up_to() {
        let end = Some(345);

        let range = immutable_file_range(None, end);

        assert_eq!(range, ImmutableFileRange::UpTo(345));
    }
}
