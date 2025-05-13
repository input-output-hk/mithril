use mithril_common::{
    entities::{SignedEntityType, SingleSignature},
    StdResult,
};

/// A signature consumer which blocks until a messages are available.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait SignatureConsumer: Sync + Send {
    /// Returns signatures when available
    async fn get_signatures(&self) -> StdResult<Vec<(SingleSignature, SignedEntityType)>>;
}
