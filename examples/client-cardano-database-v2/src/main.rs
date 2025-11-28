//! This example shows how to implement a Mithril client and use its features.
//!
//! In this example, the client interacts by default with a real aggregator (`release-preprod`) to get the data.
//!
//! A [FeedbackReceiver] using [indicatif] is used to nicely report the progress to the console.

use anyhow::{Context, anyhow};
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

use mithril_client::feedback::{FeedbackReceiver, MithrilEvent, MithrilEventCardanoDatabase};
use mithril_client::{
    AggregatorDiscoveryType, ClientBuilder, GenesisVerificationKey, MessageBuilder, MithrilError,
    MithrilResult,
};

#[derive(Parser, Debug)]
#[command(version)]
pub struct Args {
    /// Genesis verification key.
    #[clap(
        long,
        env = "GENESIS_VERIFICATION_KEY",
        default_value = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
    )]
    genesis_verification_key: String,

    /// Ancillary verification key.
    #[clap(
        long,
        env = "ANCILLARY_VERIFICATION_KEY",
        default_value = "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d"
    )]
    ancillary_verification_key: String,

    /// Aggregator endpoint URL.
    #[clap(
        long,
        env = "AGGREGATOR_ENDPOINT",
        default_value = "https://aggregator.release-preprod.api.mithril.network/aggregator"
    )]
    aggregator_endpoint: String,
}

#[tokio::main]
async fn main() -> MithrilResult<()> {
    let args = Args::parse();
    let work_dir = get_temp_dir()?;
    let progress_bar = indicatif::MultiProgress::new();
    let client = ClientBuilder::new(AggregatorDiscoveryType::Url(
        args.aggregator_endpoint.clone(),
    ))
    .set_genesis_verification_key(GenesisVerificationKey::JsonHex(
        args.genesis_verification_key.clone(),
    ))
    .set_ancillary_verification_key(args.ancillary_verification_key)
    .with_origin_tag(Some("EXAMPLE".to_string()))
    .add_feedback_receiver(Arc::new(IndicatifFeedbackReceiver::new(&progress_bar)))
    .build()?;

    let cardano_database_snapshots = client.cardano_database_v2().list().await?;

    let latest_hash = cardano_database_snapshots
        .first()
        .ok_or(anyhow!(
            "No Cardano database snapshot could be listed from aggregator: '{}'",
            args.aggregator_endpoint
        ))?
        .hash
        .as_ref();

    let cardano_database_snapshot = client.cardano_database_v2().get(latest_hash).await?.ok_or(
        anyhow!("A Cardano database should exist for hash '{latest_hash}'"),
    )?;

    let unpacked_dir = work_dir.join("unpack");
    std::fs::create_dir(&unpacked_dir).unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&cardano_database_snapshot.certificate_hash)
        .await?;

    let immutable_file_range = ImmutableFileRange::From(4000);
    let download_unpack_options = DownloadUnpackOptions {
        allow_override: true,
        include_ancillary: true,
        ..DownloadUnpackOptions::default()
    };
    client
        .cardano_database_v2()
        .download_unpack(
            &cardano_database_snapshot,
            &immutable_file_range,
            &unpacked_dir,
            download_unpack_options,
        )
        .await?;

    println!("Downloading and verifying digests file authenticity...");
    let verified_digests = client
        .cardano_database_v2()
        .download_and_verify_digests(&certificate, &cardano_database_snapshot)
        .await?;

    println!("Sending usage statistics to the aggregator...");
    let full_restoration = immutable_file_range == ImmutableFileRange::Full;
    let include_ancillary = download_unpack_options.include_ancillary;
    let number_of_immutable_files_restored =
        immutable_file_range.length(cardano_database_snapshot.beacon.immutable_file_number);
    if let Err(e) = client
        .cardano_database_v2()
        .add_statistics(
            full_restoration,
            include_ancillary,
            number_of_immutable_files_restored,
        )
        .await
    {
        println!("Could not send usage statistics to the aggregator: {e:?}");
    }

    println!("Verifying Cardano database...",);
    let allow_missing_immutables_files = false;
    let merkle_proof = client
        .cardano_database_v2()
        .verify_cardano_database(
            &certificate,
            &cardano_database_snapshot,
            &immutable_file_range,
            allow_missing_immutables_files,
            &unpacked_dir,
            &verified_digests,
        )
        .await?;

    println!(
        "Computing Cardano database snapshot '{}' message...",
        cardano_database_snapshot.hash
    );
    let message = wait_spinner(
        &progress_bar,
        MessageBuilder::new().compute_cardano_database_message(&certificate, &merkle_proof),
    )
    .await?;

    if certificate.match_message(&message) {
        println!(
            "Successfully downloaded and validated Cardano database snapshot '{}'",
            cardano_database_snapshot.hash
        );

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
    cardano_database_pb: RwLock<Option<ProgressBar>>,
    certificate_validation_pb: RwLock<Option<ProgressBar>>,
}

impl IndicatifFeedbackReceiver {
    pub fn new(progress_bar: &MultiProgress) -> Self {
        Self {
            progress_bar: progress_bar.clone(),
            cardano_database_pb: RwLock::new(None),
            certificate_validation_pb: RwLock::new(None),
        }
    }
}

#[async_trait]
impl FeedbackReceiver for IndicatifFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        match event {
            MithrilEvent::CardanoDatabase(cardano_database_event) => match cardano_database_event {
                MithrilEventCardanoDatabase::Started {
                    download_id: _,
                    total_immutable_files,
                    include_ancillary,
                } => {
                    println!("Starting download of artifact files...");
                    let size = match include_ancillary {
                        true => 1 + total_immutable_files,
                        false => total_immutable_files,
                    };
                    let pb = ProgressBar::new(size);
                    pb.set_style(ProgressStyle::with_template("{spinner:.green} {elapsed_precise}] [{wide_bar:.cyan/blue}] Files: {human_pos}/{human_len} ({eta})")
                        .unwrap()
                        .with_key("eta", |state: &ProgressState, w: &mut dyn Write| write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap())
                        .progress_chars("#>-"));
                    self.progress_bar.add(pb.clone());
                    let mut cardano_database_pb = self.cardano_database_pb.write().await;
                    *cardano_database_pb = Some(pb);
                }
                MithrilEventCardanoDatabase::Completed { .. } => {
                    let mut cardano_database_pb = self.cardano_database_pb.write().await;
                    if let Some(progress_bar) = cardano_database_pb.as_ref() {
                        progress_bar.finish_with_message("Artifact files download completed");
                    }
                    *cardano_database_pb = None;
                }
                MithrilEventCardanoDatabase::ImmutableDownloadCompleted { .. }
                | MithrilEventCardanoDatabase::AncillaryDownloadCompleted { .. } => {
                    let cardano_database_pb = self.cardano_database_pb.read().await;
                    if let Some(progress_bar) = cardano_database_pb.as_ref() {
                        progress_bar.inc(1);
                    }
                }
                _ => {
                    // Ignore other events
                }
            },
            MithrilEvent::CertificateChainValidationStarted {
                certificate_chain_validation_id: _,
            } => {
                println!("Validating certificate chain...");
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
            _ => {
                // Ignore other events
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

pub async fn wait_spinner<T, E>(
    progress_bar: &MultiProgress,
    future: impl Future<Output = Result<T, E>>,
) -> MithrilResult<T>
where
    MithrilError: From<E>,
{
    let pb = progress_bar.add(ProgressBar::new_spinner());
    let spinner = async move {
        loop {
            pb.tick();
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
    };

    tokio::select! {
        _ = spinner => Err(anyhow!("timeout")),
        res = future => res.map_err(Into::into),
    }
}
