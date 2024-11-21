use async_trait::async_trait;

use mithril_common::{crypto_helper::ProtocolInitializer, entities::Epoch, StdResult};

#[cfg_attr(test, mockall::automock)]
#[async_trait]
/// Store the ProtocolInitializer used for each Epoch. This is useful because
/// protocol parameters and stake distribution change over time.
pub trait ProtocolInitializerStorer: Sync + Send {
    /// Save a protocol initializer for the given Epoch.
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> StdResult<Option<ProtocolInitializer>>;

    /// Fetch a protocol initializer if any saved for the given Epoch.
    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<ProtocolInitializer>>;

    /// Return the list of the N last saved protocol initializers if any.
    async fn get_last_protocol_initializer(
        &self,
        last: usize,
    ) -> StdResult<Vec<(Epoch, ProtocolInitializer)>>;
}
