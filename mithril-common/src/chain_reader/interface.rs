use async_trait::async_trait;

use crate::{entities::ChainPoint, StdResult};

use super::ChainBlockNextAction;

/// The trait that reads events to either:
/// - read next block on the chain
/// - rollback to another point in case of rollback
/// - do nothing
#[async_trait]
pub trait ChainBlockReader {
    /// Get next chain block action
    async fn get_next_chain_block(
        &self,
        point: &ChainPoint,
    ) -> StdResult<Option<ChainBlockNextAction<'a>>>;
}
