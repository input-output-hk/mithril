//! The entities used when interacting with the chain.

mod chain_block_next_action;
mod datum;
mod raw_cardano_point;
mod scanned_block;

pub use chain_block_next_action::*;
pub use datum::*;
pub use raw_cardano_point::*;
pub use scanned_block::*;

/// [ChainAddress] represents an on chain address.
pub type ChainAddress = String;
