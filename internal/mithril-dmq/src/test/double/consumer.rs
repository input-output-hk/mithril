use std::{collections::VecDeque, fmt::Debug};

use tokio::sync::Mutex;

use mithril_common::{StdResult, crypto_helper::TryFromBytes, entities::PartyId};

use crate::DmqConsumerClient;

type ConsumerReturn<M> = StdResult<Vec<(M, PartyId)>>;

/// A fake implementation of the [DmqConsumerClient] trait for testing purposes.
pub struct DmqConsumerFake<M: TryFromBytes + Debug + Send + Sync> {
    results: Mutex<VecDeque<ConsumerReturn<M>>>,
}

impl<M: TryFromBytes + Debug + Send + Sync> DmqConsumerFake<M> {
    /// Creates a new `DmqConsumerFake` instance with the provided results.
    pub fn new(results: Vec<StdResult<Vec<(M, PartyId)>>>) -> Self {
        Self {
            results: Mutex::new(VecDeque::from(results)),
        }
    }
}

#[async_trait::async_trait]
impl<M: TryFromBytes + Debug + Send + Sync> DmqConsumerClient<M> for DmqConsumerFake<M> {
    async fn consume_messages(&self) -> ConsumerReturn<M> {
        let mut results = self.results.lock().await;

        results
            .pop_front()
            .ok_or_else(|| anyhow::anyhow!("No more results available in DmqConsumerFake"))?
    }
}

#[cfg(test)]
mod tests {
    use crate::test::payload::DmqMessageTestPayload;

    use super::*;

    #[tokio::test]
    async fn consume_messages_success() {
        let consumer = DmqConsumerFake::new(vec![
            Ok(vec![(
                DmqMessageTestPayload::new(b"test-1"),
                "pool-id-1".to_string(),
            )]),
            Ok(vec![(
                DmqMessageTestPayload::new(b"test-2"),
                "pool-id-2".to_string(),
            )]),
        ]);

        let messages = consumer.consume_messages().await.unwrap();

        assert_eq!(
            vec![(
                DmqMessageTestPayload::new(b"test-1"),
                "pool-id-1".to_string(),
            )],
            messages
        );
    }

    #[tokio::test]
    async fn consume_messages_failure() {
        let consumer = DmqConsumerFake::new(vec![
            Err(anyhow::anyhow!("Test error")),
            Ok(vec![(
                DmqMessageTestPayload::new(b"test-2"),
                "pool-id-2".to_string(),
            )]),
        ]);

        consumer
            .consume_messages()
            .await
            .expect_err("DmqConsumerFake should return an error");
    }
}
