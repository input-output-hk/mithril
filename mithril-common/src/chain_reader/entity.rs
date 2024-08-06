use crate::{cardano_block_scanner::ScannedBlock, entities::SlotNumber};

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
        /// The rollback slot number in the chain to read (as a new valid slot number to read from on the main chain, which has already been seen)
        slot_number: SlotNumber,
    },
}
