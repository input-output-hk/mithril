use std::{collections::HashMap, fmt::Debug, sync::Arc, time::Duration};

use blake2::{Blake2b, Digest, digest::consts::U64};
use tokio::sync::Mutex;

use mithril_common::{
    StdResult,
    crypto_helper::{TryFromBytes, TryToBytes},
    entities::PartyId,
};

use crate::{DmqConsumerClient, model::UnixTimestampProvider};

/// Default maximum time to keep a seen message in the deduplicator cache.
pub const DMQ_MESSAGE_DEDUPLICATOR_TTL: Duration = Duration::from_secs(1800);

/// Type alias for the message key used in the deduplicator cache.
type MessageKey = Vec<u8>;

/// A DMQ consumer client that filters out duplicate messages.
///
/// This implementation wraps an inner [`DmqConsumerClient`] and maintains a cache of recently seen messages.
/// When a message is consumed, if it has already been seen, the message is skipped. Otherwise, the message is passed through.
/// Expired entries are lazily cleaned up during call to `consume_messages`.
pub struct DmqConsumerClientDeduplicator<
    M: TryFromBytes + TryToBytes + Debug + Send + Sync + Clone + Eq,
> {
    inner: Arc<dyn DmqConsumerClient<M>>,
    timestamp_provider: Arc<dyn UnixTimestampProvider>,
    seen_messages: Mutex<HashMap<MessageKey, u64>>,
    ttl: Duration,
}

impl<M: TryFromBytes + TryToBytes + Debug + Send + Sync + Clone + Eq>
    DmqConsumerClientDeduplicator<M>
{
    /// Creates a new `DmqConsumerClientDeduplicator` wrapping the given inner client.
    ///
    /// The `ttl` parameter specifies how long a message is kept in the cache.
    pub fn new(
        inner: Arc<dyn DmqConsumerClient<M>>,
        timestamp_provider: Arc<dyn UnixTimestampProvider>,
        ttl: Duration,
    ) -> Self {
        Self {
            inner,
            timestamp_provider,
            seen_messages: Mutex::new(HashMap::new()),
            ttl,
        }
    }

    /// Creates a new `DmqConsumerClientDeduplicator` with the default TTL.
    pub fn new_with_default_ttl(
        inner: Arc<dyn DmqConsumerClient<M>>,
        timestamp_provider: Arc<dyn UnixTimestampProvider>,
    ) -> Self {
        Self::new(inner, timestamp_provider, DMQ_MESSAGE_DEDUPLICATOR_TTL)
    }

    /// Computes a key for the message to be used in the seen messages cache.
    fn try_compute_message_key(&self, message: &M) -> StdResult<MessageKey> {
        let mut hasher = Blake2b::<U64>::new();
        hasher.update(&message.to_bytes_vec()?);

        Ok(hasher.finalize().to_vec())
    }

    /// Gets the current timestamp from the timestamp provider.
    fn current_timestamp(&self) -> StdResult<u64> {
        self.timestamp_provider.current_timestamp()
    }

    /// Checks if a message timestamp is expired based on the current timestamp and TTL.
    fn is_expired_timestamp(&self, timestamp: u64, current_timestamp: u64) -> bool {
        current_timestamp.saturating_sub(timestamp) > self.ttl.as_secs()
    }

    /// Removes expired messages from the seen messages cache.
    async fn remove_expired_messages(&self, current_timestamp: u64) {
        let mut seen_messages = self.seen_messages.lock().await;
        seen_messages
            .retain(|_, timestamp| !self.is_expired_timestamp(*timestamp, current_timestamp));
    }

    /// Checks if a message has already been seen.
    async fn has_message_been_seen(&self, message: &M) -> StdResult<bool> {
        let seen_messages = self.seen_messages.lock().await;

        Ok(seen_messages.contains_key(&self.try_compute_message_key(message)?))
    }

    /// Marks a message as seen with the given timestamp.
    async fn mark_message_as_seen(&self, message: M, timestamp: u64) -> StdResult<()> {
        let mut seen_messages = self.seen_messages.lock().await;
        seen_messages.insert(self.try_compute_message_key(&message)?, timestamp);

        Ok(())
    }

    #[cfg(test)]
    /// Sets the expiration timestamp for all seen messages (for testing purposes).
    async fn set_seen_messages_expiration_timestamp(&self, timestamp: u64) {
        let mut seen_messages = self.seen_messages.lock().await;
        for seen_timestamp in seen_messages.values_mut() {
            *seen_timestamp = timestamp;
        }
    }

    #[cfg(test)]
    /// Returns the count of seen messages (for testing purposes).
    async fn seen_messages_count(&self) -> usize {
        self.seen_messages.lock().await.len()
    }

    #[cfg(test)]
    /// Checks if a message has been seen (for testing purposes).
    async fn has_seen_message(&self, message: &M) -> StdResult<bool> {
        Ok(self
            .seen_messages
            .lock()
            .await
            .contains_key(&self.try_compute_message_key(message)?))
    }
}

#[async_trait::async_trait]
impl<M: TryFromBytes + TryToBytes + Debug + Send + Sync + Clone + Eq + Clone> DmqConsumerClient<M>
    for DmqConsumerClientDeduplicator<M>
{
    async fn consume_messages(&self) -> StdResult<Vec<(M, PartyId)>> {
        let messages = self.inner.consume_messages().await;
        let current_timestamp = self.current_timestamp()?;
        self.remove_expired_messages(current_timestamp).await;
        let messages = messages?;

        let mut deduplicated_messages = Vec::new();
        for (message, party_id) in messages {
            if !self.has_message_been_seen(&message).await? {
                self.mark_message_as_seen(message.clone(), current_timestamp).await?;
                deduplicated_messages.push((message, party_id));
            }
        }

        Ok(deduplicated_messages)
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::test::{
        double::DmqConsumerFake, double::FakeUnixTimestampProvider, payload::DmqMessageTestPayload,
    };

    use super::*;

    fn create_deduplicator(
        inner_results: Vec<StdResult<Vec<(DmqMessageTestPayload, PartyId)>>>,
        current_timestamp: u64,
        ttl: Duration,
    ) -> DmqConsumerClientDeduplicator<DmqMessageTestPayload> {
        DmqConsumerClientDeduplicator::new(
            Arc::new(DmqConsumerFake::new(inner_results)),
            Arc::new(FakeUnixTimestampProvider::new(current_timestamp)),
            ttl,
        )
    }

    #[tokio::test]
    async fn returns_not_already_seen_messages_from_inner_client() {
        let expected_messages = vec![
            (
                DmqMessageTestPayload::new(b"message-1"),
                "party-1".to_string(),
            ),
            (
                DmqMessageTestPayload::new(b"message-2"),
                "party-2".to_string(),
            ),
        ];
        let deduplicator = create_deduplicator(
            vec![Ok(expected_messages.clone())],
            1000,
            Duration::from_secs(600),
        );

        let messages = deduplicator.consume_messages().await.unwrap();

        assert_eq!(expected_messages, messages);
    }

    #[tokio::test]
    async fn returns_nothing_when_inner_returns_nothing() {
        let deduplicator = create_deduplicator(vec![Ok(vec![])], 1000, Duration::from_secs(600));

        let messages = deduplicator.consume_messages().await.unwrap();

        assert!(messages.is_empty());
    }

    #[tokio::test]
    async fn returns_error_from_failing_inner_client() {
        let deduplicator = create_deduplicator(
            vec![Err(anyhow::anyhow!("Inner client error"))],
            1000,
            Duration::from_secs(600),
        );

        let result = deduplicator.consume_messages().await;

        result.expect_err("Should return an error");
    }

    #[tokio::test]
    async fn filters_out_already_seen_messages_in_same_call() {
        let duplicate_message = DmqMessageTestPayload::new(b"duplicate");
        let unique_message = DmqMessageTestPayload::new(b"unique");
        let inner_results = vec![Ok(vec![
            (duplicate_message.clone(), "party-1".to_string()),
            (duplicate_message.clone(), "party-2".to_string()),
            (unique_message.clone(), "party-3".to_string()),
        ])];
        let deduplicator = create_deduplicator(inner_results, 1000, Duration::from_secs(600));

        let messages = deduplicator.consume_messages().await.unwrap();

        assert_eq!(
            vec![
                (duplicate_message, "party-1".to_string()),
                (unique_message, "party-3".to_string()),
            ],
            messages
        );
    }

    #[tokio::test]
    async fn filters_out_already_seen_messages_across_calls() {
        let message_1 = DmqMessageTestPayload::new(b"message-1");
        let message_2 = DmqMessageTestPayload::new(b"message-2");
        let inner_results = vec![
            Ok(vec![(message_1.clone(), "party-1".to_string())]),
            Ok(vec![
                (message_1.clone(), "party-1-duplicate".to_string()),
                (message_2.clone(), "party-2".to_string()),
            ]),
        ];
        let deduplicator = create_deduplicator(inner_results, 1000, Duration::from_secs(600));

        let batch_1 = deduplicator.consume_messages().await.unwrap();
        let batch_2 = deduplicator.consume_messages().await.unwrap();

        assert_eq!(vec![(message_1, "party-1".to_string())], batch_1);
        assert_eq!(vec![(message_2, "party-2".to_string())], batch_2);
    }

    #[tokio::test]
    async fn cleans_up_expired_entries_on_consume() {
        let message_1 = DmqMessageTestPayload::new(b"message-1");
        let message_2 = DmqMessageTestPayload::new(b"message-2");
        let deduplicator = create_deduplicator(
            vec![
                Ok(vec![(message_1.clone(), "party-1".to_string())]),
                Ok(vec![(message_2.clone(), "party-2".to_string())]),
            ],
            1000,
            Duration::from_secs(100),
        );

        deduplicator.consume_messages().await.unwrap();
        assert_eq!(1, deduplicator.seen_messages_count().await);

        deduplicator.set_seen_messages_expiration_timestamp(800).await;
        deduplicator.consume_messages().await.unwrap();

        assert_eq!(1, deduplicator.seen_messages_count().await);
        assert!(deduplicator.has_seen_message(&message_2).await.unwrap());
        assert!(!deduplicator.has_seen_message(&message_1).await.unwrap());
    }

    #[tokio::test]
    async fn cleans_up_expired_entries_when_inner_client_fails() {
        let message = DmqMessageTestPayload::new(b"message");
        let deduplicator = create_deduplicator(
            vec![
                Ok(vec![(message.clone(), "party-1".to_string())]),
                Err(anyhow::anyhow!("Inner client error")),
            ],
            1000,
            Duration::from_secs(100),
        );

        deduplicator.consume_messages().await.unwrap();
        assert_eq!(1, deduplicator.seen_messages_count().await);

        deduplicator.set_seen_messages_expiration_timestamp(800).await;
        deduplicator
            .consume_messages()
            .await
            .expect_err("Should return an error");

        assert_eq!(0, deduplicator.seen_messages_count().await);
    }
}
