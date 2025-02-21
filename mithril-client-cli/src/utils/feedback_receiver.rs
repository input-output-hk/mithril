use async_trait::async_trait;
use indicatif::{ProgressBar, ProgressDrawTarget};
use slog::Logger;
use tokio::sync::RwLock;

use super::{
    DownloadProgressReporter, MultiDownloadProgressReporter, ProgressBarKind, ProgressOutputType,
};

use mithril_client::feedback::{FeedbackReceiver, MithrilEvent, MithrilEventCardanoDatabase};

/// Custom [FeedbackReceiver] for Cardano DB to handle events sent
/// by the `mithril-client` library
pub struct IndicatifFeedbackReceiver {
    download_progress_reporter: RwLock<Option<DownloadProgressReporter>>,
    certificate_validation_pb: RwLock<Option<ProgressBar>>,
    cardano_database_multi_pb: RwLock<Option<MultiDownloadProgressReporter>>,
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
                let mut download_progress_reporter = self.download_progress_reporter.write().await;
                *download_progress_reporter = Some(DownloadProgressReporter::new(
                    pb,
                    self.output_type,
                    ProgressBarKind::Bytes,
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
                    total_immutable_files,
                    include_ancillary,
                } => {
                    let multi_pb = MultiDownloadProgressReporter::new(
                        total_immutable_files + if include_ancillary { 1 } else { 0 },
                        self.output_type,
                        self.logger.clone(),
                    );
                    let mut cardano_database_multi_pb =
                        self.cardano_database_multi_pb.write().await;
                    *cardano_database_multi_pb = Some(multi_pb);
                }
                MithrilEventCardanoDatabase::Completed { download_id: _ } => {
                    let mut cardano_database_multi_pb =
                        self.cardano_database_multi_pb.write().await;

                    if let Some(multi_pb) = cardano_database_multi_pb.as_ref() {
                        multi_pb.finish_all("Cardano DB download completed").await;
                        *cardano_database_multi_pb = None;
                    }
                }
                MithrilEventCardanoDatabase::DigestDownloadStarted { .. }
                | MithrilEventCardanoDatabase::DigestDownloadProgress { .. }
                | MithrilEventCardanoDatabase::DigestDownloadCompleted { .. }
                | MithrilEventCardanoDatabase::ImmutableDownloadStarted { .. }
                | MithrilEventCardanoDatabase::ImmutableDownloadProgress { .. } => {
                    // Ignore those events as those downloads are fast enough that we don't need to show progress bars
                }
                MithrilEventCardanoDatabase::ImmutableDownloadCompleted {
                    immutable_file_number: _,
                    download_id: _,
                } => {
                    if let Some(cardano_database_multi_pb) =
                        self.cardano_database_multi_pb.read().await.as_ref()
                    {
                        cardano_database_multi_pb.bump_main_bar_progress();
                    }
                }
                MithrilEventCardanoDatabase::AncillaryDownloadStarted {
                    download_id: _,
                    size,
                } => {
                    if let Some(cardano_database_multi_pb) =
                        self.cardano_database_multi_pb.read().await.as_ref()
                    {
                        cardano_database_multi_pb
                            .add_child_bar("Ancillary", ProgressBarKind::Bytes, size)
                            .await;
                    }
                }
                MithrilEventCardanoDatabase::AncillaryDownloadProgress {
                    download_id: _,
                    downloaded_bytes,
                    size: _,
                } => {
                    if let Some(cardano_database_multi_pb) =
                        self.cardano_database_multi_pb.read().await.as_ref()
                    {
                        cardano_database_multi_pb
                            .progress_child_bar("Ancillary", downloaded_bytes)
                            .await;
                    }
                }
                MithrilEventCardanoDatabase::AncillaryDownloadCompleted { download_id: _ } => {
                    if let Some(cardano_database_multi_pb) =
                        self.cardano_database_multi_pb.read().await.as_ref()
                    {
                        cardano_database_multi_pb
                            .finish_child_bar("Ancillary")
                            .await;
                    }
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
        (cardano_db, immutable_dl, started => $sender:expr, immutable:$immutable_file_number:expr, size:$size:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::ImmutableDownloadStarted {
                        immutable_file_number: $immutable_file_number,
                        download_id: DOWNLOAD_ID.to_string(),
                        size: $size,
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
        (cardano_db, ancillary_dl, started => $sender:expr, size:$size:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadStarted {
                        download_id: DOWNLOAD_ID.to_string(),
                        size: $size,
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
        (cardano_db, digests_dl, started => $sender:expr, size:$size:expr) => {
            $sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::DigestDownloadStarted {
                        download_id: DOWNLOAD_ID.to_string(),
                        size: $size,
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
        async fn start_including_ancillary_add_one_to_total_downloads() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:99, ancillary:true);

            assert_eq!(
                receiver
                    .cardano_database_multi_pb
                    .read()
                    .await
                    .as_ref()
                    .map(|pb| pb.total_downloads()),
                Some(100)
            );
        }

        #[tokio::test]
        async fn starting_twice_should_supersede_first_multi_progress_bar() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:50, ancillary:false);
            send_event!(cardano_db, dl_started => sender, total:99, ancillary:false);

            assert_eq!(
                receiver
                    .cardano_database_multi_pb
                    .read()
                    .await
                    .as_ref()
                    .map(|pb| pb.total_downloads()),
                Some(99)
            );
        }

        #[tokio::test]
        async fn complete_without_start_should_not_panic() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_completed => sender);
        }

        #[tokio::test]
        async fn starting_immutable_downloads_should_not_add_progress_bars() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:50, ancillary:false);

            let multi_pb_option = receiver.cardano_database_multi_pb.read().await;
            let multi_pb = multi_pb_option.as_ref().unwrap();

            assert_eq!(multi_pb.number_of_active_downloads().await, 0);
            send_event!(cardano_db, immutable_dl, started => sender, immutable:1, size:123);
            assert_eq!(multi_pb.number_of_active_downloads().await, 0);
        }

        #[tokio::test]
        async fn completed_immutable_downloads_bump_progress() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:50, ancillary:false);

            let multi_pb_option = receiver.cardano_database_multi_pb.read().await;
            let multi_pb = multi_pb_option.as_ref().unwrap();

            assert_eq!(multi_pb.position(), 0);
            send_event!(cardano_db, immutable_dl, completed => sender, immutable:24);
            assert_eq!(multi_pb.position(), 1);
        }

        #[tokio::test]
        async fn starting_digests_downloads_should_not_add_progress_bars() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:50, ancillary:false);

            let multi_pb_option = receiver.cardano_database_multi_pb.read().await;
            let multi_pb = multi_pb_option.as_ref().unwrap();

            assert_eq!(multi_pb.number_of_active_downloads().await, 0);
            send_event!(cardano_db, digests_dl, started => sender, size:789);
            assert_eq!(multi_pb.number_of_active_downloads().await, 0);
        }

        #[tokio::test]
        async fn starting_ancillary_downloads_should_add_a_progress_bar() {
            let receiver = build_feedback_receiver(ProgressOutputType::Hidden);
            let sender = FeedbackSender::new(&[receiver.clone()]);

            send_event!(cardano_db, dl_started => sender, total:50, ancillary:false);

            let multi_pb_option = receiver.cardano_database_multi_pb.read().await;
            let multi_pb = multi_pb_option.as_ref().unwrap();

            assert_eq!(multi_pb.number_of_active_downloads().await, 0);
            send_event!(cardano_db, ancillary_dl, started => sender, size:456);
            assert_eq!(multi_pb.number_of_active_downloads().await, 1);
        }
    }
}
