//! This example shows how to implement a Mithril client and use its features.
//!
//! In this example, the client interacts by default with a real aggregator (`testing-preview`) to get the data.
//!
//! A [FeedbackReceiver] using [indicatif] is used to nicely report the progress to the console.

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use clap::Parser;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar, ProgressState, ProgressStyle};
use mithril_client::cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange};
use std::fmt::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;

use mithril_client::feedback::{FeedbackReceiver, MithrilEvent};
use mithril_client::{ClientBuilder, MessageBuilder, MithrilResult};

#[derive(Parser, Debug)]
#[command(version)]
pub struct Args {
    /// Genesis verification key.
    #[clap(
        long,
        env = "GENESIS_VERIFICATION_KEY",
        default_value = "5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d"
    )]
    genesis_verification_key: String,

    /// Aggregator endpoint URL.
    #[clap(
        long,
        env = "AGGREGATOR_ENDPOINT",
        default_value = "http://localhost:8080/aggregator"
    )]
    aggregator_endpoint: String,
}

#[tokio::main]
async fn main() -> MithrilResult<()> {
    let args = Args::parse();
    let work_dir = get_temp_dir()?;
    let progress_bar = indicatif::MultiProgress::new();
    let client =
        ClientBuilder::aggregator(&args.aggregator_endpoint, &args.genesis_verification_key)
            .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(&progress_bar)))
            .build()?;

    let cardano_database_snapshots = client.cardano_database().list().await?;

    let latest_hash = cardano_database_snapshots
        .first()
        .ok_or(anyhow!(
            "No Cardano database snapshot could be listed from aggregator: '{}'",
            args.aggregator_endpoint
        ))?
        .hash
        .as_ref();

    let cardano_database_snapshot =
        client
            .cardano_database()
            .get(latest_hash)
            .await?
            .ok_or(anyhow!(
                "A Cardano database should exist for hash '{latest_hash}'"
            ))?;

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&cardano_database_snapshot.certificate_hash)
        .await?;

    let immutable_file_range = ImmutableFileRange::Range(10, 11);
    let download_unpack_options = DownloadUnpackOptions {
        allow_override: true,
        include_ancillary: true,
    };
    client
        .cardano_database()
        .download_unpack(
            &cardano_database_snapshot,
            &immutable_file_range,
            &unpacked_dir,
            download_unpack_options,
        )
        .await?;

    println!("Computing Cardano database Merkle proof...",);
    let merkle_proof = client
        .cardano_database()
        .compute_merkle_proof(
            &certificate,
            &cardano_database_snapshot,
            &immutable_file_range,
            &unpacked_dir,
        )
        .await?;
    merkle_proof
        .verify()
        .with_context(|| "Merkle proof verification failed")?;

    //TODO: Add statistics

    println!(
        "Computing Cardano database snapshot '{}' message ...",
        cardano_database_snapshot.hash
    );
    let message = wait_spinner(
        &progress_bar,
        MessageBuilder::new().compute_cardano_database_message(&certificate, &merkle_proof),
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
            MithrilEvent::ImmutableDownloadStarted {
                immutable_file_number,
                download_id: _,
            } => {
                let size = 1_000_000;
                println!("Starting download of immutable files '{immutable_file_number:0>5}'");
                let pb = ProgressBar::new(size);
                pb.set_style(ProgressStyle::with_template("{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {bytes}/{total_bytes} ({eta})")
                    .unwrap()
                    .with_key("eta", |state: &ProgressState, w: &mut dyn Write| write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap())
                    .progress_chars("#>-"));
                self.progress_bar.add(pb.clone());
                let mut download_pb = self.download_pb.write().await;
                *download_pb = Some(pb);
            }
            MithrilEvent::ImmutableDownloadProgress {
                download_id: _,
                downloaded_bytes,
                size: _,
            } => {
                let download_pb = self.download_pb.read().await;
                if let Some(progress_bar) = download_pb.as_ref() {
                    progress_bar.set_position(downloaded_bytes);
                }
            }
            MithrilEvent::ImmutableDownloadCompleted { download_id: _ } => {
                let mut download_pb = self.download_pb.write().await;
                if let Some(progress_bar) = download_pb.as_ref() {
                    progress_bar.finish_with_message("Immutable download completed");
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
            MithrilEvent::CertificateFetchedFromCache {
                certificate_chain_validation_id: _,
                certificate_hash,
            } => {
                let certificate_validation_pb = self.certificate_validation_pb.read().await;
                if let Some(progress_bar) = certificate_validation_pb.as_ref() {
                    progress_bar.set_message(format!("Cached '{certificate_hash}'"));
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
            _ => {
                //println!("Unexpected event: {:?}", event)
            }
        }
    }
}

fn get_temp_dir() -> MithrilResult<PathBuf> {
    let dir = std::env::temp_dir()
        .join("mithril_examples")
        .join("cardano_database_snapshot");

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
