use serde::{Deserialize, Serialize};

/// [Cardano Slot number](https://docs.cardano.org/learn/cardano-node/#slotsandepochs)
pub type SlotNumber = u64;

/// BlockNumber is the block number of a Cardano transaction.
pub type BlockNumber = u64;

/// Hash of a Cardano Block
pub type BlockHash = String;

///The Cardano chain point which is used to identify a specific point in the Cardano chain.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ChainPoint {
    /// The slot number
    pub slot_number: SlotNumber,

    ///  The block number
    pub block_number: BlockNumber,

    /// The hex encoded block hash
    pub block_hash: BlockHash,
}
