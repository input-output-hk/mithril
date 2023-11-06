use async_trait::async_trait;
use std::sync::{Arc, RwLock};

/// Event that can be reported by a [FeedbackReceiver].
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MithrilEvent {
    /// A snapshot download has started
    SnapshotDownloadStarted {
        /// Size of the downloaded archive
        size: u64,
    },
    /// A snapshot download has completed
    SnapshotDownloadComplete,
}

/// A sender of [MithrilEvent].
#[derive(Clone)]
pub struct FeedbackSender {
    receiver: Arc<dyn FeedbackReceiver>,
}

impl FeedbackSender {
    /// Create a new [FeedbackSender].
    pub fn new(receiver: Arc<dyn FeedbackReceiver>) -> FeedbackSender {
        Self { receiver }
    }

    /// Send the given event to the known receiver.
    pub async fn send_event(&self, event: MithrilEvent) {
        self.receiver.handle_event(event).await;
    }
}

/// A receiver of [MithrilEvent].
#[async_trait]
pub trait FeedbackReceiver: Sync + Send {
    /// Callback called by a [FeedbackSender] when it need to send an [event][MithrilEvent].
    async fn handle_event(&self, event: MithrilEvent);
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
        let sender = FeedbackSender::new(receiver.clone());

        sender
            .send_event(SnapshotDownloadStarted { size: 10 })
            .await;
        sender.send_event(SnapshotDownloadComplete).await;

        assert_eq!(
            receiver.stacked_events(),
            vec![
                SnapshotDownloadStarted { size: 10 },
                SnapshotDownloadComplete
            ]
        );
    }

    #[tokio::test]
    async fn send_event_multiple_thread() {
        let receiver = Arc::new(StackFeedbackReceiver::new());
        let sender = FeedbackSender::new(receiver.clone());
        let sender2 = sender.clone();
        let mut join_set = JoinSet::new();

        join_set.spawn(async move {
            // Step 1:
            sender.send_event(SnapshotDownloadStarted { size: 1 }).await;
            tokio::time::sleep(Duration::from_millis(2)).await;
            // Step 3:
            sender.send_event(SnapshotDownloadComplete).await;
            sender.send_event(SnapshotDownloadStarted { size: 2 }).await;
        });

        join_set.spawn(async move {
            // Step 2:
            sender2.send_event(SnapshotDownloadComplete).await;
            sender2
                .send_event(SnapshotDownloadStarted { size: 3 })
                .await;
            tokio::time::sleep(Duration::from_millis(5)).await;
            // Step 4:
            sender2.send_event(SnapshotDownloadComplete).await;
        });

        while let Some(res) = join_set.join_next().await {
            res.unwrap();
        }

        assert_eq!(
            receiver.stacked_events(),
            vec![
                SnapshotDownloadStarted { size: 1 },
                SnapshotDownloadComplete,
                SnapshotDownloadStarted { size: 3 },
                SnapshotDownloadComplete,
                SnapshotDownloadStarted { size: 2 },
                SnapshotDownloadComplete,
            ]
        );
    }
}
