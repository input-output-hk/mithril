use std::{collections::VecDeque, fmt::Debug, marker::PhantomData};

use tokio::sync::Mutex;

use mithril_common::{crypto_helper::TryToBytes, StdResult};

use crate::DmqPublisher;

/// A fake implementation of the [DmqPublisher] trait for testing purposes.
pub struct DmqPublisherFake<M: TryToBytes + Debug + Send + Sync> {
    results: Mutex<VecDeque<StdResult<()>>>,
    phantom: PhantomData<M>,
}

impl<M: TryToBytes + Debug + Send + Sync> DmqPublisherFake<M> {
    /// Creates a new `DmqPublisherFake` instance with the provided results.
    pub fn new(results: Vec<StdResult<()>>) -> Self {
        Self {
            results: Mutex::new(VecDeque::from(results)),
            phantom: PhantomData,
        }
    }
}

#[async_trait::async_trait]
impl<M: TryToBytes + Debug + Send + Sync> DmqPublisher<M> for DmqPublisherFake<M> {
    async fn publish_message(&self, _message: M) -> StdResult<()> {
        let mut results = self.results.lock().await;

        results
            .pop_front()
            .ok_or_else(|| anyhow::anyhow!("No more results available in DmqPublisherFake"))?
    }
}

#[cfg(test)]
mod tests {
    use crate::test::payload::DmqMessageTestPayload;

    use super::*;

    #[tokio::test]
    async fn publish_messages_success() {
        let publisher = DmqPublisherFake::new(vec![Ok(()), Err(anyhow::anyhow!("Test error"))]);

        publisher
            .publish_message(DmqMessageTestPayload::new(b"test-1"))
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn publish_messages_failure() {
        let publisher = DmqPublisherFake::new(vec![Err(anyhow::anyhow!("Test error")), Ok(())]);

        publisher
            .publish_message(DmqMessageTestPayload::new(b"test-1"))
            .await
            .expect_err("DmqPublisherFake should return an error");
    }
}
