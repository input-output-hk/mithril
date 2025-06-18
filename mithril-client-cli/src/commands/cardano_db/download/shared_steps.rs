use anyhow::Context;
use chrono::Utc;
use std::path::Path;

use mithril_client::{Client, MithrilCertificate, MithrilResult};

use crate::utils::ProgressPrinter;

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

pub fn log_download_information(
    db_dir: &Path,
    snapshot_hash: &str,
    cardano_network: &str,
    cardano_node_version: &str,
    json_output: bool,
    include_ancillary: bool,
) -> MithrilResult<()> {
    let canonicalized_filepath = &db_dir.canonicalize().with_context(|| {
        format!(
            "Could not get canonicalized filepath of '{}'",
            db_dir.display()
        )
    })?;

    let docker_cmd = format!(
        "docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source=\"{}\",target=/data/db/ -e NETWORK={} ghcr.io/intersectmbo/cardano-node:{}",
        canonicalized_filepath.display(),
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
                "db_directory": canonicalized_filepath,
                "run_docker_cmd": docker_cmd,
                "snapshot_converter_cmd_to_lmdb": snapshot_converter_cmd("LMDB"),
                "snapshot_converter_cmd_to_legacy": snapshot_converter_cmd("Legacy")
            })
        } else {
            serde_json::json!({
                "timestamp": Utc::now().to_rfc3339(),
                "db_directory": canonicalized_filepath,
                "run_docker_cmd": docker_cmd
            })
        };

        println!("{}", json);
    } else {
        println!(
            r###"Cardano database snapshot '{}' archives have been successfully unpacked. Immutable files have been successfully checked against Mithril multi-signature contained in the certificate.

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
