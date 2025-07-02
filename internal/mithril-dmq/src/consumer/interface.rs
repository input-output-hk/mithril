use std::fmt::Debug;

use mithril_common::{StdResult, crypto_helper::TryFromBytes, entities::PartyId};

/// Trait for consuming messages from a DMQ node.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait DmqConsumer<M: TryFromBytes + Debug + Send + Sync>: Send + Sync {
    /// Consume messages from the DMQ node.
    async fn consume_messages(&self) -> StdResult<Vec<(M, PartyId)>>;
}
