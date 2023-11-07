use async_trait::async_trait;
use slog::{info, Logger};
use std::sync::{Arc, RwLock};
use uuid::Uuid;

/// Event that can be reported by a [FeedbackReceiver].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MithrilEvent {
    /// A snapshot download has started
    SnapshotDownloadStarted {
        /// Digest of the downloaded snapshot
        digest: String,
        /// Unique identifier used to track this specific snapshot download
        download_id: String,
        /// Size of the downloaded archive
        size: u64,
    },
    /// A snapshot download is in progress
    SnapshotDownloadProgress {
        /// Unique identifier used to track this specific snapshot download
        download_id: String,
        /// Number of bytes that have been downloaded
        downloaded_bytes: u64,
        /// Size of the downloaded archive
        size: u64,
    },
    /// A snapshot download has completed
    SnapshotDownloadComplete {
        /// Unique identifier used to track this specific snapshot download
        download_id: String,
    },
}

impl MithrilEvent {
    /// Generate a random unique identifier to identify a snapshot download
    pub fn new_download_id() -> String {
        Uuid::new_v4().to_string()
    }
}

/// A sender of [MithrilEvent].
///
/// It uses Arc internally so it can be cloned at will.
#[derive(Clone)]
pub struct FeedbackSender {
    receivers: Vec<Arc<dyn FeedbackReceiver>>,
}

impl FeedbackSender {
    /// Create a new [FeedbackSender].
    pub fn new(receivers: &[Arc<dyn FeedbackReceiver>]) -> FeedbackSender {
        Self {
            receivers: receivers.to_vec(),
        }
    }

    /// Send the given event to the known receiver.
    pub async fn send_event(&self, event: MithrilEvent) {
        for receiver in &self.receivers {
            receiver.handle_event(event.clone()).await;
        }
    }
}

/// A receiver of [MithrilEvent].
#[async_trait]
pub trait FeedbackReceiver: Sync + Send {
    /// Callback called by a [FeedbackSender] when it need to send an [event][MithrilEvent].
    async fn handle_event(&self, event: MithrilEvent);
}

/// A [FeedbackReceiver] that write the event it receives in a [slog logger][Logger].
pub struct SlogFeedbackReceiver {
    logger: Logger,
}

impl SlogFeedbackReceiver {
    /// Create a new [SlogFeedbackReceiver].
    pub fn new(logger: Logger) -> SlogFeedbackReceiver {
        Self { logger }
    }
}

#[async_trait]
impl FeedbackReceiver for SlogFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        match event {
            MithrilEvent::SnapshotDownloadStarted {
                digest,
                download_id,
                size,
            } => {
                info!(
                    self.logger,
                    "Snapshot download started";
                    "size" => size,
                    "digest" => digest,
                    "download_id" => download_id,
                );
            }
            MithrilEvent::SnapshotDownloadProgress {
                download_id,
                downloaded_bytes,
                size,
            } => {
                info!(
                    self.logger,
                    "Snapshot download in progress ...";
                    "downloaded bytes" => downloaded_bytes,
                    "size" => size,
                    "download_id" => download_id,
                );
            }
            MithrilEvent::SnapshotDownloadComplete { download_id } => {
                info!(self.logger, "Snapshot download completed"; "download_id" => download_id);
            }
        };
    }
}

/// A [FeedbackReceiver] that stack the events that it receives in a vec.
///
/// Use it only for tests purpose.
pub struct StackFeedbackReceiver {
    stacked_events: RwLock<Vec<MithrilEvent>>,
}

impl StackFeedbackReceiver {
    /// Create a new [StackFeedbackReceiver].
    pub fn new() -> StackFeedbackReceiver {
        Self {
            stacked_events: RwLock::new(vec![]),
        }
    }

    /// Returns a copy of the stored stacked events.
    ///
    /// Will crash if it can't access the stored events.
    pub fn stacked_events(&self) -> Vec<MithrilEvent> {
        let events = self.stacked_events.read().unwrap();
        events.clone()
    }
}

impl Default for StackFeedbackReceiver {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl FeedbackReceiver for StackFeedbackReceiver {
    async fn handle_event(&self, event: MithrilEvent) {
        let mut events = self.stacked_events.write().unwrap();
        events.push(event);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::feedback::MithrilEvent::{SnapshotDownloadComplete, SnapshotDownloadStarted};
    use std::time::Duration;
    use tokio::task::JoinSet;

    #[tokio::test]
    async fn send_event_same_thread() {
        let receiver = Arc::new(StackFeedbackReceiver::new());
        let sender = FeedbackSender::new(&[receiver.clone()]);

        sender
            .send_event(SnapshotDownloadStarted {
                digest: "digest".to_string(),
                download_id: "download_id".to_string(),
                size: 10,
            })
            .await;
        sender
            .send_event(SnapshotDownloadComplete {
                download_id: "download_id".to_string(),
            })
            .await;

        assert_eq!(
            receiver.stacked_events(),
            vec![
                SnapshotDownloadStarted {
                    digest: "digest".to_string(),
                    download_id: "download_id".to_string(),
                    size: 10
                },
                SnapshotDownloadComplete {
                    download_id: "download_id".to_string()
                }
            ]
        );
    }

    #[tokio::test]
    async fn send_event_multiple_thread() {
        let receiver = Arc::new(StackFeedbackReceiver::new());
        let sender = FeedbackSender::new(&[
            receiver.clone(),
            Arc::new(SlogFeedbackReceiver::new(crate::test_utils::test_logger())),
        ]);
        let sender2 = sender.clone();
        let mut join_set = JoinSet::new();

        join_set.spawn(async move {
            // Step 1:
            sender
                .send_event(SnapshotDownloadStarted {
                    digest: "digest1".to_string(),
                    download_id: "download1".to_string(),
                    size: 1,
                })
                .await;
            tokio::time::sleep(Duration::from_millis(2)).await;
            // Step 3:
            sender
                .send_event(SnapshotDownloadComplete {
                    download_id: "download3".to_string(),
                })
                .await;
            sender
                .send_event(SnapshotDownloadStarted {
                    digest: "digest2".to_string(),
                    download_id: "download2".to_string(),
                    size: 2,
                })
                .await;
        });

        join_set.spawn(async move {
            // Step 2:
            sender2
                .send_event(SnapshotDownloadComplete {
                    download_id: "download1".to_string(),
                })
                .await;
            sender2
                .send_event(SnapshotDownloadStarted {
                    digest: "digest3".to_string(),
                    download_id: "download3".to_string(),
                    size: 3,
                })
                .await;
            tokio::time::sleep(Duration::from_millis(5)).await;
            // Step 4:
            sender2
                .send_event(SnapshotDownloadComplete {
                    download_id: "download2".to_string(),
                })
                .await;
        });

        while let Some(res) = join_set.join_next().await {
            res.unwrap();
        }

        assert_eq!(
            receiver.stacked_events(),
            vec![
                SnapshotDownloadStarted {
                    digest: "digest1".to_string(),
                    download_id: "download1".to_string(),
                    size: 1
                },
                SnapshotDownloadComplete {
                    download_id: "download1".to_string()
                },
                SnapshotDownloadStarted {
                    digest: "digest3".to_string(),
                    download_id: "download3".to_string(),
                    size: 3
                },
                SnapshotDownloadComplete {
                    download_id: "download3".to_string()
                },
                SnapshotDownloadStarted {
                    digest: "digest2".to_string(),
                    download_id: "download2".to_string(),
                    size: 2
                },
                SnapshotDownloadComplete {
                    download_id: "download2".to_string()
                },
            ]
        );
    }

    #[tokio::test]
    async fn send_event_in_one_thread_and_receive_in_another_thread() {
        let receiver = Arc::new(StackFeedbackReceiver::new());
        let receiver2 = receiver.clone();
        let sender = FeedbackSender::new(&[receiver.clone()]);
        let mut join_set = JoinSet::new();

        join_set.spawn(async move {
            // Step 1:
            sender
                .send_event(SnapshotDownloadStarted {
                    digest: "digest1".to_string(),
                    download_id: "download1".to_string(),
                    size: 1,
                })
                .await;
            tokio::time::sleep(Duration::from_millis(10)).await;

            // Step 2:
            sender
                .send_event(SnapshotDownloadComplete {
                    download_id: "download1".to_string(),
                })
                .await;
            sender
                .send_event(SnapshotDownloadStarted {
                    digest: "digest2".to_string(),
                    download_id: "download2".to_string(),
                    size: 2,
                })
                .await;
            tokio::time::sleep(Duration::from_millis(10)).await;

            // Step 3:
            sender
                .send_event(SnapshotDownloadComplete {
                    download_id: "download2".to_string(),
                })
                .await;
            sender
                .send_event(SnapshotDownloadStarted {
                    digest: "digest3".to_string(),
                    download_id: "download3".to_string(),
                    size: 3,
                })
                .await;
            tokio::time::sleep(Duration::from_millis(10)).await;

            // Final step:
            sender
                .send_event(SnapshotDownloadComplete {
                    download_id: "download3".to_string(),
                })
                .await;
        });

        join_set.spawn(async move {
            // Little sleep to wait for step 1 completion
            tokio::time::sleep(Duration::from_millis(3)).await;
            assert_eq!(
                receiver2.stacked_events(),
                vec![SnapshotDownloadStarted {
                    digest: "digest1".to_string(),
                    download_id: "download1".to_string(),
                    size: 1
                },]
            );

            // Wait for step 2 completion
            tokio::time::sleep(Duration::from_millis(10)).await;
            assert_eq!(
                receiver2.stacked_events(),
                vec![
                    SnapshotDownloadStarted {
                        digest: "digest1".to_string(),
                        download_id: "download1".to_string(),
                        size: 1
                    },
                    SnapshotDownloadComplete {
                        download_id: "download1".to_string()
                    },
                    SnapshotDownloadStarted {
                        digest: "digest2".to_string(),
                        download_id: "download2".to_string(),
                        size: 2
                    },
                ]
            );

            // Wait for step 3 completion
            tokio::time::sleep(Duration::from_millis(10)).await;
            assert_eq!(
                receiver2.stacked_events(),
                vec![
                    SnapshotDownloadStarted {
                        digest: "digest1".to_string(),
                        download_id: "download1".to_string(),
                        size: 1
                    },
                    SnapshotDownloadComplete {
                        download_id: "download1".to_string()
                    },
                    SnapshotDownloadStarted {
                        digest: "digest2".to_string(),
                        download_id: "download2".to_string(),
                        size: 2
                    },
                    SnapshotDownloadComplete {
                        download_id: "download2".to_string()
                    },
                    SnapshotDownloadStarted {
                        digest: "digest3".to_string(),
                        download_id: "download3".to_string(),
                        size: 3
                    },
                ]
            );
        });

        while let Some(res) = join_set.join_next().await {
            res.unwrap();
        }

        assert_eq!(
            receiver.stacked_events(),
            vec![
                SnapshotDownloadStarted {
                    digest: "digest1".to_string(),
                    download_id: "download1".to_string(),
                    size: 1
                },
                SnapshotDownloadComplete {
                    download_id: "download1".to_string()
                },
                SnapshotDownloadStarted {
                    digest: "digest2".to_string(),
                    download_id: "download2".to_string(),
                    size: 2
                },
                SnapshotDownloadComplete {
                    download_id: "download2".to_string()
                },
                SnapshotDownloadStarted {
                    digest: "digest3".to_string(),
                    download_id: "download3".to_string(),
                    size: 3
                },
                SnapshotDownloadComplete {
                    download_id: "download3".to_string()
                },
            ]
        );
    }
}
