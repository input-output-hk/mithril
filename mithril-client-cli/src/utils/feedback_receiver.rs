use async_trait::async_trait;
use indicatif::{ProgressBar, ProgressDrawTarget, ProgressState, ProgressStyle};
use slog::Logger;
use std::fmt::Write;
use tokio::sync::RwLock;

use super::{DownloadProgressReporter, ProgressOutputType};

use mithril_client::feedback::{FeedbackReceiver, MithrilEvent, MithrilEventCardanoDatabase};

/// Custom [FeedbackReceiver] for Cardano DB to handle events sent
/// by the `mithril-client` library
pub struct IndicatifFeedbackReceiver {
    download_progress_reporter: RwLock<Option<DownloadProgressReporter>>,
    certificate_validation_pb: RwLock<Option<ProgressBar>>,
    output_type: ProgressOutputType,
    logger: Logger,
}

impl IndicatifFeedbackReceiver {
    /// [IndicatifFeedbackReceiver] constructor
    pub fn new(output_type: ProgressOutputType, logger: Logger) -> Self {
        Self {
            download_progress_reporter: RwLock::new(None),
            certificate_validation_pb: RwLock::new(None),
            output_type,
            logger,
        }
    }
}

#[async_trait]
impl FeedbackReceiver for IndicatifFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        match event {
            MithrilEvent::SnapshotDownloadStarted {
                digest: _,
                download_id: _,
                size,
            } => {
                let pb = if self.output_type == ProgressOutputType::Tty {
                    ProgressBar::new(size)
                } else {
                    ProgressBar::with_draw_target(Some(size), ProgressDrawTarget::hidden())
                };
                pb.set_style(ProgressStyle::with_template("{spinner:.green} [{elapsed_precise}] [{wide_bar:.cyan/blue}] {bytes}/{total_bytes} ({eta})")
                    .unwrap()
                    .with_key("eta", |state : &ProgressState, w: &mut dyn Write| write!(w, "{:.1}s", state.eta().as_secs_f64()).unwrap())
                    .progress_chars("#>-"));
                let mut download_progress_reporter = self.download_progress_reporter.write().await;
                *download_progress_reporter = Some(DownloadProgressReporter::new(
                    pb,
                    self.output_type,
                    self.logger.clone(),
                ));
            }
            MithrilEvent::SnapshotDownloadProgress {
                download_id: _,
                downloaded_bytes,
                size: _,
            } => {
                let download_progress_reporter = self.download_progress_reporter.read().await;
                if let Some(progress_reporter) = download_progress_reporter.as_ref() {
                    progress_reporter.report(downloaded_bytes);
                }
            }
            MithrilEvent::SnapshotDownloadCompleted { download_id: _ } => {
                let mut download_progress_reporter = self.download_progress_reporter.write().await;
                if let Some(progress_reporter) = download_progress_reporter.as_ref() {
                    progress_reporter.finish("Cardano DB download completed");
                }
                *download_progress_reporter = None;
            }
            MithrilEvent::CardanoDatabase(cardano_database_event) => match cardano_database_event {
                MithrilEventCardanoDatabase::Started { download_id: _ } => {
                    println!("MithrilEventCardanoDatabase::Started")
                }
                MithrilEventCardanoDatabase::Completed { download_id: _ } => {
                    println!("MithrilEventCardanoDatabase::Completed")
                }
                MithrilEventCardanoDatabase::ImmutableDownloadStarted {
                    immutable_file_number: _,
                    download_id: _,
                } => {
                    println!("MithrilEventCardanoDatabase::ImmutableDownloadStarted")
                }
                MithrilEventCardanoDatabase::ImmutableDownloadProgress {
                    immutable_file_number: _,
                    download_id: _,
                    downloaded_bytes: _,
                    size: _,
                } => {
                    println!("MithrilEventCardanoDatabase::ImmutableDownloadProgress")
                }
                MithrilEventCardanoDatabase::ImmutableDownloadCompleted {
                    immutable_file_number: _,
                    download_id: _,
                } => {
                    println!("MithrilEventCardanoDatabase::ImmutableDownloadCompleted")
                }
                MithrilEventCardanoDatabase::AncillaryDownloadStarted { download_id: _ } => {
                    println!("MithrilEventCardanoDatabase::AncillaryDownloadStarted")
                }
                MithrilEventCardanoDatabase::AncillaryDownloadProgress {
                    download_id: _,
                    downloaded_bytes: _,
                    size: _,
                } => {
                    println!("MithrilEventCardanoDatabase::AncillaryDownloadProgress")
                }
                MithrilEventCardanoDatabase::AncillaryDownloadCompleted { download_id: _ } => {
                    println!("MithrilEventCardanoDatabase::AncillaryDownloadCompleted")
                }
                MithrilEventCardanoDatabase::DigestDownloadStarted { download_id: _ } => {
                    println!("MithrilEventCardanoDatabase::DigestDownloadStarted")
                }
                MithrilEventCardanoDatabase::DigestDownloadProgress {
                    download_id: _,
                    downloaded_bytes: _,
                    size: _,
                } => {
                    println!("MithrilEventCardanoDatabase::DigestDownloadProgress")
                }
                MithrilEventCardanoDatabase::DigestDownloadCompleted { download_id: _ } => {
                    println!("MithrilEventCardanoDatabase::DigestDownloadCompleted")
                }
            },
            MithrilEvent::CertificateChainValidationStarted {
                certificate_chain_validation_id: _,
            } => {
                let pb = if self.output_type == ProgressOutputType::Tty {
                    ProgressBar::new_spinner()
                } else {
                    ProgressBar::hidden()
                };
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
        }
    }
}
