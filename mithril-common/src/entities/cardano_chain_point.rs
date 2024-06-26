use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

cfg_fs! {
    use pallas_network::miniprotocols::{chainsync::Tip, Point};
}

use crate::signable_builder::Beacon;

/// [Cardano Slot number](https://docs.cardano.org/learn/cardano-node/#slotsandepochs)
pub type SlotNumber = u64;

/// BlockNumber is the block number of a Cardano transaction.
pub type BlockNumber = u64;

impl Beacon for BlockNumber {}

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

impl ChainPoint {
    /// [ChainPoint] factory
    pub fn new<T: Into<BlockHash>>(
        slot_number: SlotNumber,
        block_number: BlockNumber,
        block_hash: T,
    ) -> ChainPoint {
        ChainPoint {
            slot_number,
            block_number,
            block_hash: block_hash.into(),
        }
    }

    /// Create a new origin chain point
    pub fn origin() -> ChainPoint {
        ChainPoint {
            slot_number: 0,
            block_number: 0,
            block_hash: String::new(),
        }
    }

    /// Check if origin chain point
    pub fn is_origin(&self) -> bool {
        self.slot_number == 0 && self.block_number == 0 && self.block_hash.is_empty()
    }

    cfg_test_tools! {
        /// Create a dummy ChainPoint
        pub fn dummy() -> Self {
            Self {
                slot_number: 100,
                block_number: 50,
                block_hash: "block_hash-50".to_string(),
            }
        }
    }
}

impl Display for ChainPoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ChainPoint (slot_number: {}, block_number: {}, block_hash: {})",
            self.slot_number, self.block_number, self.block_hash
        )
    }
}

impl PartialOrd for ChainPoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ChainPoint {
    fn cmp(&self, other: &Self) -> Ordering {
        self.block_number
            .cmp(&other.block_number)
            .then(self.slot_number.cmp(&other.slot_number))
            .then(self.block_hash.cmp(&other.block_hash))
    }
}

cfg_fs! {
    impl From<ChainPoint> for Point {
        fn from(chain_point: ChainPoint) -> Self {
            match chain_point.is_origin() {
                true => Self::Origin,
                false => Self::Specific(
                    chain_point.slot_number,
                    hex::decode(&chain_point.block_hash).unwrap(), // TODO: keep block_hash as a Vec<u8>
                ),
            }
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
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::*;

    #[test]
    fn chain_point_ord_cmp_block_number_take_precedence_over_other_fields() {
        let chain_point1 = ChainPoint {
            slot_number: 15,
            block_number: 10,
            block_hash: "hash2".to_string(),
        };
        let chain_point2 = ChainPoint {
            slot_number: 5,
            block_number: 20,
            block_hash: "hash1".to_string(),
        };

        assert_eq!(Ordering::Less, chain_point1.cmp(&chain_point2));
    }

    #[test]
    fn chain_point_ord_cmp_if_block_number_equals_then_compare_slot_numbers() {
        let chain_point1 = ChainPoint {
            slot_number: 15,
            block_number: 0,
            block_hash: "hash2".to_string(),
        };
        let chain_point2 = ChainPoint {
            slot_number: 5,
            block_number: 0,
            block_hash: "hash1".to_string(),
        };

        assert_eq!(Ordering::Greater, chain_point1.cmp(&chain_point2));
    }

    #[test]
    fn chain_point_ord_cmp_if_block_number_and_slot_number_equals_then_compare_block_hash() {
        let chain_point1 = ChainPoint {
            slot_number: 5,
            block_number: 10,
            block_hash: "hash1".to_string(),
        };
        let chain_point2 = ChainPoint {
            slot_number: 5,
            block_number: 10,
            block_hash: "hash2".to_string(),
        };

        assert_eq!(Ordering::Less, chain_point1.cmp(&chain_point2));
    }
}
