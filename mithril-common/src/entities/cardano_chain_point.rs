use pallas_network::miniprotocols::{chainsync::Tip, Point};
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

impl From<ChainPoint> for Point {
    fn from(chain_point: ChainPoint) -> Self {
        Point::Specific(
            chain_point.slot_number,
            hex::decode(&chain_point.block_hash).unwrap(), // TODO: keep block_hash as a Vec<u8>
        )
    }
}

impl From<Point> for ChainPoint {
    fn from(point: Point) -> Self {
        match point {
            Point::Specific(slot_number, block_hash) => Self {
                slot_number,
                block_number: 0,
                block_hash: hex::encode(block_hash),
            },
            Point::Origin => Self {
                slot_number: 0,
                block_number: 0,
                block_hash: String::new(),
            },
        }
    }
}

impl From<Tip> for ChainPoint {
    fn from(tip: Tip) -> Self {
        let chain_point: Self = tip.0.into();
        Self {
            slot_number: chain_point.slot_number,
            block_number: tip.1,
            block_hash: chain_point.block_hash,
        }
    }
}

impl From<ChainPoint> for Tip {
    fn from(chain_point: ChainPoint) -> Self {
        let block_number = chain_point.block_number;
        let point: Point = chain_point.into();
        Tip(point, block_number)
    }
}
