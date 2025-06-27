use mithril_common::{
    StdResult,
    entities::{SignedEntityType, SingleSignature},
};

/// A signature consumer which blocks until messages are available.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait SignatureConsumer: Sync + Send {
    /// Returns signatures when available
    async fn get_signatures(&self) -> StdResult<Vec<(SingleSignature, SignedEntityType)>>;

    /// Returns the origin tag of the consumer (e.g. HTTP or DMQ)
    fn get_origin_tag(&self) -> String;
}
