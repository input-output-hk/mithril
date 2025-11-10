use mithril_common::StdResult;
use mithril_common::entities::{Epoch, Signer};

/// Publishes a signer registration to a third party.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait SignerRegistrationPublisher: Send + Sync {
    /// Registers signer with the aggregator.
    async fn register_signer(&self, epoch: Epoch, signer: &Signer) -> StdResult<()>;
}
