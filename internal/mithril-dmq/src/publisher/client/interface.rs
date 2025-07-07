use mithril_common::{StdResult, crypto_helper::TryToBytes};

/// Trait for publishing messages from a DMQ node.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait DmqPublisher<M: TryToBytes + Send + Sync>: Send + Sync {
    /// Publishes a message to the DMQ node.
    async fn publish_message(&self, message: M) -> StdResult<()>;
}
