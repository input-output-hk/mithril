#[cfg(feature = "fs")]
use pallas_network::miniprotocols::Point as PallasPoint;
use std::fmt::{Debug, Formatter};

use crate::cardano_block_scanner::ScannedBlock;
use crate::entities::{ChainPoint, SlotNumber};

/// Point in the chain that can be intersected.
///
/// Internally the point used in Cardano doesn't have a block number like our [ChainPoint]
/// does, so we need to use a different struct to represent it. Else converting from one to the other
/// would be lossy.
#[derive(Clone, PartialEq)]
pub struct RawCardanoPoint {
    /// The [slot number](https://docs.cardano.org/learn/cardano-node/#slotsandepochs)
    pub slot_number: SlotNumber,

    /// Hex array of the block hash
    pub block_hash: Vec<u8>,
}

impl RawCardanoPoint {
    /// Instantiate a new `RawCardanoPoint`
    pub fn new<T: Into<Vec<u8>>>(slot_number: SlotNumber, block_hash: T) -> Self {
        RawCardanoPoint {
            slot_number,
            block_hash: block_hash.into(),
        }
    }

    /// Create a new origin `RawCardanoPoint`
    pub fn origin() -> Self {
        RawCardanoPoint {
            slot_number: SlotNumber(0),
            block_hash: Vec::new(),
        }
    }

    /// Check if origin
    pub fn is_origin(&self) -> bool {
        self.slot_number == 0 && self.block_hash.is_empty()
    }
}

impl From<&ChainPoint> for RawCardanoPoint {
    fn from(point: &ChainPoint) -> Self {
        RawCardanoPoint {
            slot_number: point.slot_number,
            block_hash: hex::decode(&point.block_hash).unwrap(),
        }
    }
}

impl From<ChainPoint> for RawCardanoPoint {
    fn from(point: ChainPoint) -> Self {
        Self::from(&point)
    }
}

impl Debug for RawCardanoPoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug = f.debug_struct("RawCardanoPoint");
        debug
            .field("slot_number", &self.slot_number)
            .field("block_hash", &hex::encode(&self.block_hash))
            .finish()
    }
}

cfg_fs! {
    impl From<RawCardanoPoint> for PallasPoint {
        fn from(raw_point: RawCardanoPoint) -> Self {
            match raw_point.is_origin() {
                true => Self::Origin,
                false => Self::Specific(
                    *raw_point.slot_number,
                    raw_point.block_hash
                ),
            }
        }
    }

    impl From<PallasPoint> for RawCardanoPoint {
        fn from(point: PallasPoint) -> Self {
            match point {
                PallasPoint::Specific(slot_number, block_hash) => Self {
                    slot_number: SlotNumber(slot_number),
                    block_hash,
                },
                PallasPoint::Origin => Self::origin(),
            }
        }
    }
}

impl From<&ScannedBlock> for RawCardanoPoint {
    fn from(scanned_block: &ScannedBlock) -> Self {
        RawCardanoPoint {
            slot_number: scanned_block.slot_number,
            block_hash: scanned_block.block_hash.clone(),
        }
    }
}

impl From<ScannedBlock> for RawCardanoPoint {
    fn from(scanned_block: ScannedBlock) -> Self {
        Self::from(&scanned_block)
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::BlockNumber;

    use super::*;

    #[test]
    fn from_chain_point_to_raw_cardano_point_conversions() {
        let expected_hash = vec![4, 2, 12, 9, 7];
        let chain_point =
            ChainPoint::new(SlotNumber(8), BlockNumber(23), hex::encode(&expected_hash));

        assert_eq!(
            RawCardanoPoint::new(SlotNumber(8), expected_hash.clone()),
            RawCardanoPoint::from(&chain_point)
        );
        assert_eq!(
            RawCardanoPoint::new(SlotNumber(8), expected_hash.clone()),
            RawCardanoPoint::from(chain_point)
        );
    }

    #[test]
    fn from_scanned_block_to_raw_cardano_point_conversions() {
        let expected_hash = vec![7, 1, 13, 7, 8];
        let scanned_block = ScannedBlock::new(
            expected_hash.clone(),
            BlockNumber(31),
            SlotNumber(4),
            Vec::<&str>::new(),
        );
        assert_eq!(
            RawCardanoPoint::new(SlotNumber(4), expected_hash.clone()),
            RawCardanoPoint::from(&scanned_block)
        );
        assert_eq!(
            RawCardanoPoint::new(SlotNumber(4), expected_hash.clone()),
            RawCardanoPoint::from(scanned_block)
        );
    }
}
