use mithril_common::StdResult;

/// Trait for the server side of consuming messages from a DMQ node.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait DmqConsumerServer: Send + Sync {
    /// Processes the next message received from the DMQ network.
    async fn process_message(&self) -> StdResult<()>;

    /// Runs the DMQ consumer server.
    async fn run(&self) -> StdResult<()>;
}
