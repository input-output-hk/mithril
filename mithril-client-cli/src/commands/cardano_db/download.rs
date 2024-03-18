use anyhow::{anyhow, Context};
use chrono::Utc;
use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog_scope::{debug, warn};
use std::{
    collections::HashMap,
    fs::File,
    path::{Path, PathBuf},
    sync::Arc,
};

use crate::{
    commands::client_builder,
    configuration::ConfigParameters,
    utils::{
        CardanoDbDownloadChecker, CardanoDbUtils, ExpanderUtils, IndicatifFeedbackReceiver,
        ProgressOutputType, ProgressPrinter,
    },
};
use mithril_client::{
    common::ProtocolMessage, Client, MessageBuilder, MithrilCertificate, MithrilResult, Snapshot,
};

/// Clap command to download a Cardano db and verify its associated certificate.
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbDownloadCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Digest of the cardano db to download. Use the `list` command to get that information.
    ///
    /// If `latest` is specified as digest, the command will return the latest cardano db.
    digest: String,

    /// Directory where the cardano db will be downloaded. By default, a
    /// subdirectory will be created in this directory to extract and verify the
    /// certificate.
    #[clap(long)]
    download_dir: Option<PathBuf>,

    /// Genesis Verification Key to check the certificate chain.
    #[clap(long, env = "GENESIS_VERIFICATION_KEY")]
    genesis_verification_key: Option<String>,
}

impl CardanoDbDownloadCommand {
    /// Command execution
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        let config = config_builder.add_source(self.clone()).build()?;
        let params = ConfigParameters::new(config.try_deserialize::<HashMap<String, String>>()?);
        let download_dir: &String = &params.require("download_dir")?;
        let db_dir = Path::new(download_dir).join("db");

        let progress_output_type = if self.json {
            ProgressOutputType::JsonReporter
        } else {
            ProgressOutputType::Tty
        };
        let progress_printer = ProgressPrinter::new(progress_output_type, 5);
        let client = client_builder(&params)?
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(
                progress_output_type,
            )))
            .build()?;

        let get_list_of_artifact_ids = || async {
            let cardano_dbs = client.snapshot().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest cardano db digest"
            })?;

            Ok(cardano_dbs
                .iter()
                .map(|cardano_db| cardano_db.digest.to_owned())
                .collect::<Vec<String>>())
        };

        let cardano_db_message = client
            .snapshot()
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
            3,
            &progress_printer,
            &client,
            &cardano_db_message,
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
            5,
            &progress_printer,
            &certificate,
            &message,
            &cardano_db_message,
            &db_dir,
        )
        .await?;

        Self::log_download_information(&db_dir, &cardano_db_message, self.json)?;

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
        if let Err(e) = CardanoDbDownloadChecker::check_prerequisites(
            db_dir,
            cardano_db.size,
            cardano_db.compression_algorithm.unwrap_or_default(),
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

    async fn download_and_unpack_cardano_db(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        client: &Client,
        cardano_db: &Snapshot,
        db_dir: &Path,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Downloading and unpacking the cardano db")?;
        client
            .snapshot()
            .download_unpack(cardano_db, db_dir)
            .await?;

        // The cardano db download does not fail if the statistic call fails.
        // It would be nice to implement tests to verify the behavior of `add_statistics`
        if let Err(e) = client.snapshot().add_statistics(cardano_db).await {
            warn!("Could not increment cardano db download statistics: {e:?}");
        }

        // Append 'clean' file to speedup node bootstrap
        if let Err(error) = File::create(db_dir.join("clean")) {
            warn!(
                "Could not create clean shutdown marker file in directory {}: {error}",
                db_dir.display()
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
            format!(
                "Can not compute the cardano db message from the directory: '{:?}'",
                db_dir
            )
        })?;

        Ok(message)
    }

    async fn verify_cardano_db_signature(
        step_number: u16,
        progress_printer: &ProgressPrinter,
        certificate: &MithrilCertificate,
        message: &ProtocolMessage,
        cardano_db: &Snapshot,
        db_dir: &Path,
    ) -> MithrilResult<()> {
        progress_printer.report_step(step_number, "Verifying the cardano db signature…")?;
        if !certificate.match_message(message) {
            debug!("Digest verification failed, removing unpacked files & directory.");

            if let Err(error) = std::fs::remove_dir_all(db_dir) {
                warn!("Error while removing unpacked files & directory: {error}.");
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
            let cardano_node_version = cardano_db
                .cardano_node_version
                .clone()
                .unwrap_or("latest".to_string());
            println!(
                r###"Cardano db '{}' has been unpacked and successfully checked against Mithril multi-signature contained in the certificate.
                    
    Files in the directory '{}' can be used to run a Cardano node with version >= {}.
    
    If you are using Cardano Docker image, you can restore a Cardano Node with:
    
    docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="{}",target=/data/db/ -e NETWORK={} ghcr.io/intersectmbo/cardano-node:{}
    
    "###,
                cardano_db.digest,
                db_dir.display(),
                cardano_node_version,
                canonicalized_filepath.display(),
                cardano_db.beacon.network,
                cardano_node_version
            );
        }

        Ok(())
    }
}

impl Source for CardanoDbDownloadCommand {
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

#[cfg(test)]
mod tests {
    use mithril_client::{
        common::{CardanoDbBeacon, ProtocolMessagePartKey},
        MithrilCertificateMetadata,
    };
    use mithril_common::entities::SignedEntityType;
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
        let beacon = CardanoDbBeacon::new("testnet".to_string(), 10, 100);

        MithrilCertificate {
            hash: "hash".to_string(),
            previous_hash: "previous_hash".to_string(),
            beacon: beacon.clone(),
            metadata: MithrilCertificateMetadata::dummy(),
            protocol_message: protocol_message.clone(),
            signed_message: "signed_message".to_string(),
            aggregate_verification_key: String::new(),
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(beacon),
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

        let result = CardanoDbDownloadCommand::verify_cardano_db_signature(
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
