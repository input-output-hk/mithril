use async_trait::async_trait;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget, ProgressState, ProgressStyle};
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
    cardano_database_multi_pb: RwLock<Option<MultiProgress>>,
    output_type: ProgressOutputType,
    logger: Logger,
}

impl IndicatifFeedbackReceiver {
    /// [IndicatifFeedbackReceiver] constructor
    pub fn new(output_type: ProgressOutputType, logger: Logger) -> Self {
        Self {
            download_progress_reporter: RwLock::new(None),
            certificate_validation_pb: RwLock::new(None),
            cardano_database_multi_pb: RwLock::new(None),
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
                MithrilEventCardanoDatabase::Started {
                    download_id: _,
                    total_immutable_files: _,
                    include_ancillary: _,
                } => {
                    let multi_pb = MultiProgress::new();
                    let mut cardano_database_multi_pb =
                        self.cardano_database_multi_pb.write().await;
                    *cardano_database_multi_pb = Some(multi_pb);
                }
                MithrilEventCardanoDatabase::Completed { download_id: _ } => {
                    let mut cardano_database_multi_pb =
                        self.cardano_database_multi_pb.write().await;
                    *cardano_database_multi_pb = None;
                }
                MithrilEventCardanoDatabase::ImmutableDownloadStarted {
                    immutable_file_number: _,
                    download_id: _,
                    size: _,
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
                MithrilEventCardanoDatabase::AncillaryDownloadStarted {
                    download_id: _,
                    size: _,
                } => {
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
                MithrilEventCardanoDatabase::DigestDownloadStarted {
                    download_id: _,
                    size: _,
                } => {
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

#[cfg(test)]
mod tests {
    use slog::o;
    use std::sync::Arc;

    use mithril_client::feedback::FeedbackSender;

    use super::*;

    const DOWNLOAD_ID: &str = "id";

    macro_rules! send_event {
        (cardano_db, dl_started => $sender:expr, total:$total_immutable:expr, ancillary:$include_ancillary:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::Started {
                        download_id: DOWNLOAD_ID.to_string(),
                        total_immutable_files: $total_immutable,
                        include_ancillary: $include_ancillary,
                    },
                ))
                .await;
        };
        (cardano_db, dl_completed => $sender:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::Completed {
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
        (cardano_db, immutable_dl, started => $sender:expr, immutable:$immutable_file_number:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::ImmutableDownloadStarted {
                        immutable_file_number: $immutable_file_number,
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
        (cardano_db, immutable_dl, progress => $sender:expr, immutable:$immutable_file_number:expr, bytes:$downloaded_bytes:expr, size:$size:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::ImmutableDownloadProgress {
                        immutable_file_number: $immutable_file_number,
                        download_id: DOWNLOAD_ID.to_string(),
                        downloaded_bytes: $downloaded_bytes,
                        size: $size,
                    },
                ))
                .await;
        };
        (cardano_db, immutable_dl, completed => $sender:expr, immutable:$immutable_file_number:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::ImmutableDownloadCompleted {
                        immutable_file_number: $immutable_file_number,
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
        (cardano_db, ancillary_dl, started => $sender:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadStarted {
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
        (cardano_db, ancillary_dl, progress => $sender:expr, bytes:$downloaded_bytes:expr, size:$size:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadProgress {
                        download_id: DOWNLOAD_ID.to_string(),
                        downloaded_bytes: $downloaded_bytes,
                        size: $size,
                    },
                ))
                .await;
        };
        (cardano_db, ancillary_dl, completed => $sender:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadCompleted {
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
        (cardano_db, digests_dl, started => $sender:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::DigestDownloadStarted {
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
        (cardano_db, digests_dl, progress => $sender:expr, bytes:$downloaded_bytes:expr, size:$size:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::DigestDownloadProgress {
                        download_id: DOWNLOAD_ID.to_string(),
                        downloaded_bytes: $downloaded_bytes,
                        size: $size,
                    },
                ))
                .await;
        };
        (cardano_db, digests_dl, completed => $sender:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::DigestDownloadCompleted {
                        download_id: DOWNLOAD_ID.to_string(),
                    },
                ))
                .await;
        };
    }

    fn build_feedback_receiver(output_type: ProgressOutputType) -> Arc<IndicatifFeedbackReceiver> {
        Arc::new(IndicatifFeedbackReceiver::new(
            output_type,
            slog::Logger::root(slog::Discard, o!()),
        ))
    }

    mod cardano_database {
        use super::*;

        #[tokio::test]
        async fn starting_should_add_multi_progress_bar() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:99, ancillary:false);

            assert!(receiver.cardano_database_multi_pb.read().await.is_some());
        }

        #[tokio::test]
        async fn start_then_complete_should_remove_multi_progress_bar() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:99, ancillary:false);
            send_event!(cardano_db, dl_completed => sender);

            assert!(receiver.cardano_database_multi_pb.read().await.is_none());
        }

        #[tokio::test]
        async fn starting_twice_should_supersede_first_multi_progress_bar() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:99, ancillary:false);
            send_event!(cardano_db, dl_started => sender, total:99, ancillary:false);
            //send_event!(cardano_db, immutable_dl, progress => sender, immutable:2, bytes:12, size:43);

            // todo
        }

        #[tokio::test]
        async fn complete_without_start_should_not_panic() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_completed => sender);
        }
    }
}
