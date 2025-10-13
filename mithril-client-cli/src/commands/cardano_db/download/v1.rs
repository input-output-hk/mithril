use anyhow::{Context, anyhow};
use slog::{Logger, debug, warn};
use std::{fs::File, path::Path, sync::Arc};

use mithril_client::{
    MessageBuilder, MithrilCertificate, MithrilResult, Snapshot, common::ProtocolMessage,
    snapshot_client::SnapshotClient,
};

use crate::{
    CommandContext,
    commands::{
        cardano_db::{download::DB_DIRECTORY_NAME, shared_steps},
        client_builder,
    },
    utils::{
        CardanoDbDownloadChecker, CardanoDbUtils, ExpanderUtils, IndicatifFeedbackReceiver,
        ProgressOutputType, ProgressPrinter,
    },
};

#[derive(Debug, Clone)]
pub(super) struct PreparedCardanoDbV1Download {
    pub(super) digest: String,
    pub(super) download_dir: String,
    pub(super) include_ancillary: bool,
    pub(super) ancillary_verification_key: Option<String>,
}

impl PreparedCardanoDbV1Download {
    /// Command execution
    pub async fn execute(&self, context: &CommandContext) -> MithrilResult<()> {
        let db_dir = Path::new(&self.download_dir).join(DB_DIRECTORY_NAME);

        let progress_output_type = if context.is_json_output_enabled() {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 5);
        let client = client_builder(context.config_parameters())?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
                context.logger().clone(),
            )))
            .set_ancillary_verification_key(self.ancillary_verification_key.clone())
            .with_logger(context.logger().clone())
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

        let certificate = shared_steps::fetch_certificate_and_verifying_chain(
            2,
            &progress_printer,
            &client,
            &cardano_db_message.certificate_hash,
        )
        .await?;

        Self::download_and_unpack_cardano_db(
            context.logger(),
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
            context.logger(),
            5,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db_message,
            &db_dir,
        )
        .await?;

        shared_steps::log_download_information(
            &db_dir,
            &cardano_db_message.digest,
            &cardano_db_message.network,
            &cardano_db_message.cardano_node_version,
            context.is_json_output_enabled(),
            self.include_ancillary,
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
            snapshot_client.download_unpack_full(cardano_db, db_dir).await?;
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
}

#[cfg(test)]
mod tests {
    use mithril_client::{
        MithrilCertificateMetadata,
        common::{CardanoDbBeacon, ProtocolMessagePartKey, SignedEntityType, test::Dummy},
    };
    use mithril_common::test::TempDir;

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

        let result = PreparedCardanoDbV1Download::verify_cardano_db_signature(
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
