use async_trait::async_trait;

use crate::cardano_block_scanner::RawCardanoPoint;
use crate::StdResult;

use super::ChainBlockNextAction;

/// The trait that reads events to either:
/// - read next block on the chain
/// - rollback to another point in case of rollback
/// - do nothing when tip of the chain is reached
#[async_trait]
pub trait ChainBlockReader: Send + Sync {
    /// Sets the chain point
    async fn set_chain_point(&mut self, point: &RawCardanoPoint) -> StdResult<()>;

    /// Get the next chain block
    async fn get_next_chain_block(&mut self) -> StdResult<Option<ChainBlockNextAction>>;
}
