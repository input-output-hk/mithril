use std::fmt::Debug;

use mithril_common::{crypto_helper::TryToBytes, StdResult};

/// Trait for publishing messages from a DMQ node.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait DmqPublisher<M: TryToBytes + Debug + Send + Sync>: Send + Sync {
    /// Publishes a message to the DMQ node.
    async fn publish_message(&self, message: M) -> StdResult<()>;
}
