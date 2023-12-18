//! This example shows how to implement a Mithril client and use its features.
//!
//! In this example, the client interacts with a real aggregator (`testing-preview`) to get the data.
//!
//! A [FeedbackReceiver] using [indicatif] is used to nicely report the progress to the console.

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar, ProgressState, ProgressStyle};
use mithril_client::feedback::{FeedbackReceiver, MithrilEvent};
use mithril_client::{ClientBuilder, MessageBuilder, MithrilResult};
use std::fmt::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

#[tokio::main]
async fn main() -> MithrilResult<()> {
    let aggregator_endpoint = "https://aggregator.testing-preview.api.mithril.network/aggregator";
    let genesis_verification_key = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
    let work_dir = get_temp_dir()?;
    let progress_bar = indicatif::MultiProgress::new();
    let client = ClientBuilder::aggregator(aggregator_endpoint, genesis_verification_key)
        .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(&progress_bar)))
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
        .ok_or(anyhow!("A snapshot should exist for hash '{last_digest}'"))?;

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await?;

    client
        .snapshot()
        .download_unpack(&snapshot, &unpacked_dir)
        .await?;

    if let Err(e) = client.snapshot().add_statistics(&snapshot).await {
        println!("Could not POST snapshot download statistics: {:?}", e);
    }

    println!("Computing snapshot '{}' message ...", snapshot.digest);
    let message = wait_spinner(
        &progress_bar,
        MessageBuilder::new().compute_snapshot_message(&certificate, &unpacked_dir),
    )
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

pub struct IndicatifFeedbackReceiver {
    progress_bar: MultiProgress,
    download_pb: RwLock<Option<ProgressBar>>,
    certificate_validation_pb: RwLock<Option<ProgressBar>>,
}

impl IndicatifFeedbackReceiver {
    pub fn new(progress_bar: &MultiProgress) -> Self {
        Self {
            progress_bar: progress_bar.clone(),
            download_pb: RwLock::new(None),
            certificate_validation_pb: RwLock::new(None),
        }
    }
}

#[async_trait]
impl FeedbackReceiver for IndicatifFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        match event {
            MithrilEvent::SnapshotDownloadStarted {
                digest,
                download_id: _,
                size,
            } => {
                println!("Starting download of snapshot '{digest}'");
                let pb = ProgressBar::new(size);
                pb.set_style(ProgressStyle::with_template("{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {bytes}/{total_bytes} ({eta})")
                    .unwrap()
                    .with_key("eta", |state: &ProgressState, w: &mut dyn Write| write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap())
                    .progress_chars("#>-"));
                self.progress_bar.add(pb.clone());
                let mut download_pb = self.download_pb.write().await;
                *download_pb = Some(pb);
            }
            MithrilEvent::SnapshotDownloadProgress {
                download_id: _,
                downloaded_bytes,
                size: _,
            } => {
                let download_pb = self.download_pb.read().await;
                if let Some(progress_bar) = download_pb.as_ref() {
                    progress_bar.set_position(downloaded_bytes);
                }
            }
            MithrilEvent::SnapshotDownloadCompleted { download_id: _ } => {
                let mut download_pb = self.download_pb.write().await;
                if let Some(progress_bar) = download_pb.as_ref() {
                    progress_bar.finish_with_message("Snapshot download completed");
                }
                *download_pb = None;
            }
            MithrilEvent::CertificateChainValidationStarted {
                certificate_chain_validation_id: _,
            } => {
                println!("Validating certificate chain ...");
                let pb = ProgressBar::new_spinner();
                self.progress_bar.add(pb.clone());
                let mut certificate_validation_pb = self.certificate_validation_pb.write().await;
                *certificate_validation_pb = Some(pb);
            }
            MithrilEvent::CertificateValidated {
                certificate_chain_validation_id: _,
                certificate_hash,
            } => {
                let certificate_validation_pb = self.certificate_validation_pb.read().await;
                if let Some(progress_bar) = certificate_validation_pb.as_ref() {
                    progress_bar.set_message(format!("Certificate '{certificate_hash}' is valid"));
                    progress_bar.inc(1);
                }
            }
            MithrilEvent::CertificateChainValidated {
                certificate_chain_validation_id: _,
            } => {
                let mut certificate_validation_pb = self.certificate_validation_pb.write().await;
                if let Some(progress_bar) = certificate_validation_pb.as_ref() {
                    progress_bar.finish_with_message("Certificate chain validated");
                }
                *certificate_validation_pb = None;
            }
        }
    }
}

fn get_temp_dir() -> MithrilResult<PathBuf> {
    let dir = std::env::temp_dir()
        .join("mithril_examples")
        .join("client_snapshot");

    if dir.exists() {
        std::fs::remove_dir_all(&dir).with_context(|| format!("Could not remove dir {dir:?}"))?;
    }
    std::fs::create_dir_all(&dir).with_context(|| format!("Could not create dir {dir:?}"))?;

    Ok(dir)
}

async fn wait_spinner<T>(
    progress_bar: &MultiProgress,
    future: impl Future<Output = MithrilResult<T>>,
) -> MithrilResult<T> {
    let pb = progress_bar.add(ProgressBar::new_spinner());
    let spinner = async move {
        loop {
            pb.tick();
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
    };

    tokio::select! {
        _ = spinner => Err(anyhow!("timeout")),
        res = future => res,
    }
}
