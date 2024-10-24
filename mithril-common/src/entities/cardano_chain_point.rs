use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

use crate::entities::{BlockNumber, SlotNumber};

/// Hash of a Cardano Block
pub type BlockHash = String;

/// The Cardano chain point which is used to identify a specific point in the Cardano chain.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ChainPoint {
    /// The [slot number](https://docs.cardano.org/learn/cardano-node/#slotsandepochs)
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

    cfg_test_tools! {
        /// Create a dummy ChainPoint
        pub fn dummy() -> Self {
            Self {
                slot_number: SlotNumber(100),
                block_number: BlockNumber(0),
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

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::*;

    #[test]
    fn chain_point_ord_cmp_block_number_take_precedence_over_other_fields() {
        let chain_point1 = ChainPoint {
            slot_number: SlotNumber(15),
            block_number: BlockNumber(10),
            block_hash: "hash2".to_string(),
        };
        let chain_point2 = ChainPoint {
            slot_number: SlotNumber(5),
            block_number: BlockNumber(20),
            block_hash: "hash1".to_string(),
        };

        assert_eq!(Ordering::Less, chain_point1.cmp(&chain_point2));
    }

    #[test]
    fn chain_point_ord_cmp_if_block_number_equals_then_compare_slot_numbers() {
        let chain_point1 = ChainPoint {
            slot_number: SlotNumber(15),
            block_number: BlockNumber(0),
            block_hash: "hash2".to_string(),
        };
        let chain_point2 = ChainPoint {
            slot_number: SlotNumber(5),
            block_number: BlockNumber(0),
            block_hash: "hash1".to_string(),
        };

        assert_eq!(Ordering::Greater, chain_point1.cmp(&chain_point2));
    }

    #[test]
    fn chain_point_ord_cmp_if_block_number_and_slot_number_equals_then_compare_block_hash() {
        let chain_point1 = ChainPoint {
            slot_number: SlotNumber(5),
            block_number: BlockNumber(10),
            block_hash: "hash1".to_string(),
        };
        let chain_point2 = ChainPoint {
            slot_number: SlotNumber(5),
            block_number: BlockNumber(10),
            block_hash: "hash2".to_string(),
        };

        assert_eq!(Ordering::Less, chain_point1.cmp(&chain_point2));
    }
}
