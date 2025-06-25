use anyhow::{Context, anyhow};
use chrono::Utc;
use slog::{Logger, debug, warn};
use std::path::Path;

use mithril_client::{
    CardanoDatabaseSnapshot, Client, MessageBuilder, MithrilCertificate, MithrilResult,
    cardano_database_client::ImmutableFileRange,
    common::{ImmutableFileNumber, MKProof, ProtocolMessage},
};

use crate::utils::{CardanoDbUtils, ProgressPrinter};

pub async fn fetch_certificate_and_verifying_chain(
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

/// Computes the Cardano database snapshot message using the provided certificate and Merkle proof.
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

/// Verifies that the Cardano database snapshot matches the associated certificate.
pub async fn verify_message_matches_certificate(
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

pub fn log_download_information(
    db_dir: &Path,
    snapshot_hash: &str,
    cardano_network: &str,
    cardano_node_version: &str,
    json_output: bool,
    include_ancillary: bool,
) -> MithrilResult<()> {
    let canonical_filepath = &db_dir
        .canonicalize()
        .with_context(|| format!("Could not get canonical filepath of '{}'", db_dir.display()))?;

    let docker_cmd = format!(
        "docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source=\"{}\",target=/data/db/ -e NETWORK={} ghcr.io/intersectmbo/cardano-node:{}",
        canonical_filepath.display(),
        cardano_network,
        cardano_node_version
    );

    let snapshot_converter_cmd = |flavor| {
        format!(
            "mithril-client --unstable tools utxo-hd snapshot-converter --db-directory {} --cardano-node-version {} --utxo-hd-flavor {} --commit",
            db_dir.display(),
            cardano_node_version,
            flavor,
        )
    };

    if json_output {
        let json = if include_ancillary {
            serde_json::json!({
                "timestamp": Utc::now().to_rfc3339(),
                "db_directory": canonical_filepath,
                "run_docker_cmd": docker_cmd,
                "snapshot_converter_cmd_to_lmdb": snapshot_converter_cmd("LMDB"),
                "snapshot_converter_cmd_to_legacy": snapshot_converter_cmd("Legacy")
            })
        } else {
            serde_json::json!({
                "timestamp": Utc::now().to_rfc3339(),
                "db_directory": canonical_filepath,
                "run_docker_cmd": docker_cmd
            })
        };

        println!("{json}");
    } else {
        println!(
            r###"Cardano database snapshot '{}' archives have been successfully unpacked. Immutable files have been successfully verified with Mithril.

    Files in the directory '{}' can be used to run a Cardano node with version >= {cardano_node_version}.

    If you are using Cardano Docker image, you can restore a Cardano Node with:

    {}

    "###,
            snapshot_hash,
            db_dir.display(),
            docker_cmd
        );

        if include_ancillary {
            println!(
                r###"Upgrade and replace the restored ledger state snapshot to 'LMDB' flavor by running the command:

    {}

    Or to 'Legacy' flavor by running the command:

    {}

    "###,
                snapshot_converter_cmd("LMDB"),
                snapshot_converter_cmd("Legacy"),
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use mithril_client::{
        MithrilCertificateMetadata,
        common::{CardanoDbBeacon, ProtocolMessagePartKey, SignedEntityType},
    };
    use mithril_common::test_utils::TempDir;

    use crate::utils::ProgressOutputType;

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
    async fn verify_message_matches_certificate_should_remove_db_dir_if_messages_mismatch() {
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
            "verify_message_matches_certificate_should_remove_db_dir_if_messages_mismatch",
        );

        let result = verify_message_matches_certificate(
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
