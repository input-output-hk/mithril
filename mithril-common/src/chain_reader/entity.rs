use crate::{cardano_block_scanner::ScannedBlock, entities::ChainPoint};

/// The action that indicates what to do next when scanning the chain
#[derive(Debug, Clone, PartialEq)]
pub enum ChainBlockNextAction {
    /// RollForward event (we are still on the correct fork)
    RollForward {
        /// The parsed chain block
        parsed_block: ScannedBlock,
    },
    /// RollBackward event (we are on an incorrect fork, we need to get back a point to roll forward again)
    RollBackward {
        /// The rollback chain point in the chain to read (as a new valid chain point to read from on the main chain, which has already been seen)
        chain_point: ChainPoint,
    },
}
