use pallas_traverse::MultiEraBlock;

use crate::entities::ChainPoint;

/// The parsed chain block representation
pub type ParsedBlock<'a> = MultiEraBlock<'a>;

/// The action that indicates what to do next when scanning the chain
pub enum ChainBlockNextAction<'a> {
    /// RollForward event (we are still on the correct fork)
    RollForward {
        /// The next point in the chain to read
        next_point: ChainPoint,
        /// The parsed block
        block: ParsedBlock<'a>,
    },
    /// RollBackward event (we are on an incorrect fork, we need to get back a point to roll forward again)
    RollBackward {
        /// The rollback point in the chain to read (as a new valid point to read from on the main chain, which has already been seen)
        rollback_point: ChainPoint,
    },
}
