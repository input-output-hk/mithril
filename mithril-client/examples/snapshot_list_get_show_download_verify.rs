//! This example shows how to implement a Mithril Client and use its features.
//! In this example, the client interacts with a real aggregator (testing-preview) to get the data.
//! Some tasks can take some time (download_unpack, verify_chain and compute_snapshot_message).

use anyhow::{anyhow, Context};
use mithril_client::feedback::SlogFeedbackReceiver;
use mithril_client::{ClientBuilder, MessageBuilder, MithrilResult};
use std::path::PathBuf;
use std::sync::Arc;

#[tokio::main]
async fn main() -> MithrilResult<()> {
    let aggregator_endpoint = "https://aggregator.testing-preview.api.mithril.network/aggregator";
    let genesis_verification_key = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
    let work_dir = get_temp_dir()?;
    let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
        .add_feedback_receiver(Arc::new(SlogFeedbackReceiver::new(build_logger())))
        .build()?;

    let snapshots = client.snapshot().list().await?;

    let last_digest = snapshots
        .first()
        .ok_or(anyhow!(
            "No snapshots could be listed from aggregator: '{aggregator_endpoint}'"
        ))?
        .digest
        .as_ref();

    let snapshot = client
        .snapshot()
        .get(last_digest)
        .await?
        .ok_or(anyhow!("A Snapshot should exist for hash '{last_digest}'"))?;

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    client
        .snapshot()
        .download_unpack(&snapshot, &unpacked_dir)
        .await?;

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await?;

    let message = MessageBuilder::new()
        .compute_snapshot_message(&certificate, &unpacked_dir)
        .await?;

    if certificate.match_message(&message) {
        Ok(())
    } else {
        Err(anyhow::anyhow!(
            "Certificate and message did not match:\ncertificate_message: '{}'\n computed_message: '{}'",
            certificate.signed_message,
            message.compute_hash()
        ))
    }
}

fn get_temp_dir() -> MithrilResult<PathBuf> {
    let dir = std::env::temp_dir()
        .join("mithril_examples")
        .join("snapshot_list_get_show_download_verify");

    if dir.exists() {
        std::fs::remove_dir_all(&dir).with_context(|| format!("Could not remove dir {dir:?}"))?;
    }
    std::fs::create_dir_all(&dir).with_context(|| format!("Could not create dir {dir:?}"))?;

    Ok(dir)
}

fn build_logger() -> slog::Logger {
    use slog::Drain;

    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    slog::Logger::root(Arc::new(drain), slog::o!())
}
