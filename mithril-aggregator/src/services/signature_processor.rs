use mithril_common::StdResult;

/// A signature processor which receives signature and processes them.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait SignatureProcessor: Sync + Send {
    /// Processes the signatures received from the consumer.
    async fn process_signatures(&self) -> StdResult<()>;

    /// Starts the processor, which will run indefinitely, processing signatures as they arrive.
    async fn run(&self) -> StdResult<()> {
        loop {
            self.process_signatures().await?;
        }
    }

    /// Stops the processor. This method should be called to gracefully shut down the processor.
    async fn stop(&self) -> StdResult<()>;
}
