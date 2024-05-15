use pallas_network::miniprotocols::chainsync::BlockContent;

use crate::entities::ChainPoint;

/// The raw chain block representation
pub struct RawChainBlock(pub BlockContent);

/// The action that indicates what to do next when scanning the chain
pub enum ChainBlockNextAction {
    /// RollForward event (we are still on the correct fork)
    RollForward {
        /// The next point in the chain to read
        next_point: ChainPoint,
        /// The raw chain block
        raw_block: RawChainBlock,
    },
    /// RollBackward event (we are on an incorrect fork, we need to get back a point to roll forward again)
    RollBackward {
        /// The rollback point in the chain to read (as a new valid point to read from on the main chain, which has already been seen)
        rollback_point: ChainPoint,
    },
}
