use async_trait::async_trait;
use pallas_network::{facades::NodeClient, miniprotocols::chainsync};

use crate::{entities::ChainPoint, StdResult};

use super::ChainBlockNextAction;

/// The trait that reads events to either:
/// - read next block on the chain
/// - rollback to another point in case of rollback
/// - do nothing
#[async_trait]
pub trait ChainBlockReader {
    /// Process the next chain block action
    async fn process_next_chain_block(
        &mut self,
        point: chainsync::NextResponse<chainsync::BlockContent>,
    ) -> StdResult<Option<ChainBlockNextAction>>;

    /// Get the next chain block
    async fn get_next_chain_block(
        &mut self,
        point: &ChainPoint,
    ) -> StdResult<Option<ChainBlockNextAction>>;
}
