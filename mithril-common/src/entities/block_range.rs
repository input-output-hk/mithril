use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result},
    ops::{Deref, Range},
};

use crate::{
    crypto_helper::{MKMapKey, MKTreeNode},
    entities::BlockNumber,
    StdResult,
};

/// BlockRangeLength is the length of a block range.
pub type BlockRangeLength = u64;

/// BlockRange for the Cardano chain
#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Debug, Hash)]
pub struct BlockRange {
    inner_range: Range<u64>,
}

impl BlockRange {
    /// The length of the block range
    /// Important: this value should be updated with extreme care (probably with an era change) in order to avoid signing disruptions.
    pub const LENGTH: BlockRangeLength = 15;

    /// BlockRange factory
    pub fn new(start: BlockNumber, end: BlockNumber) -> Self {
        Self {
            inner_range: start..end,
        }
    }

    cfg_test_tools! {
        /// Try to add two BlockRanges
        pub fn try_add(&self, other: &BlockRange) -> StdResult<BlockRange> {
            if self.inner_range.end.max(other.inner_range.end)
                < self.inner_range.start.min(other.inner_range.start)
            {
                return Err(anyhow!(
                    "BlockRange cannot be added as they don't strictly overlap"
                ));
            }

            Ok(Self {
                inner_range: Range {
                    start: self.inner_range.start.min(other.inner_range.start),
                    end: self.inner_range.end.max(other.inner_range.end),
                },
            })
        }
    }

    /// Create a BlockRange from a block number
    pub fn from_block_number(number: BlockNumber) -> Self {
        // Unwrap is safe as the length is always strictly greater than 0
        Self::from_block_number_and_length(number, Self::LENGTH).unwrap()
    }

    /// Create a BlockRange from a block number and a range length
    pub(crate) fn from_block_number_and_length(
        number: BlockNumber,
        length: BlockRangeLength,
    ) -> StdResult<Self> {
        if length == 0 {
            return Err(anyhow!(
                "BlockRange cannot be be computed with a length of 0"
            ));
        }
        // The formula used to compute the lower bound of the block range is `⌊number / length⌋ * length`
        // The computation of the floor is done with the integer division `/` of Rust
        let block_range_start = (number / length) * length;
        let block_range_end = block_range_start + length;
        Ok(Self {
            inner_range: block_range_start..block_range_end,
        })
    }
}

impl Display for BlockRange {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "[{},{}[", self.inner_range.start, self.inner_range.end)
    }
}

impl Deref for BlockRange {
    type Target = Range<u64>;

    fn deref(&self) -> &Self::Target {
        &self.inner_range
    }
}

impl PartialOrd for BlockRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for BlockRange {
    fn cmp(&self, other: &Self) -> Ordering {
        // Order by range start, then by range end
        match self.inner_range.start.cmp(&other.inner_range.start) {
            Ordering::Equal => self.inner_range.end.cmp(&other.inner_range.end),
            order => order,
        }
    }
}

impl From<Range<u64>> for BlockRange {
    fn from(other: Range<u64>) -> Self {
        BlockRange { inner_range: other }
    }
}

impl From<BlockRange> for MKTreeNode {
    fn from(other: BlockRange) -> Self {
        let start = other.start.to_string();
        let end = other.end.to_string();
        let mut bytes = vec![];
        bytes.extend_from_slice(start.as_bytes());
        bytes.extend_from_slice("-".as_bytes());
        bytes.extend_from_slice(end.as_bytes());
        MKTreeNode::new(bytes)
    }
}

impl MKMapKey for BlockRange {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_range_cmp() {
        assert_eq!(BlockRange::new(1, 10), BlockRange::new(1, 10));
        assert_ne!(BlockRange::new(1, 10), BlockRange::new(1, 11));
        assert_ne!(BlockRange::new(1, 10), BlockRange::new(2, 10));
        assert_ne!(BlockRange::new(1, 11), BlockRange::new(2, 10));

        assert!(BlockRange::new(1, 10) < BlockRange::new(1, 11));
        assert!(BlockRange::new(1, 10) < BlockRange::new(2, 10));
        assert!(BlockRange::new(1, 11) < BlockRange::new(2, 10));
    }

    #[test]
    fn test_block_range_try_add() {
        assert_eq!(
            BlockRange::new(1, 10)
                .try_add(&BlockRange::new(1, 10))
                .unwrap(),
            BlockRange::new(1, 10)
        );
        assert_eq!(
            BlockRange::new(1, 10)
                .try_add(&BlockRange::new(1, 11))
                .unwrap(),
            BlockRange::new(1, 11)
        );
        assert_eq!(
            BlockRange::new(1, 10)
                .try_add(&BlockRange::new(2, 10))
                .unwrap(),
            BlockRange::new(1, 10)
        );
    }

    #[test]
    fn test_block_range_from_number() {
        assert_eq!(BlockRange::from_block_number(0), BlockRange::new(0, 15));
        assert_eq!(BlockRange::from_block_number(1), BlockRange::new(0, 15));
        assert_eq!(BlockRange::from_block_number(14), BlockRange::new(0, 15));
        assert_eq!(BlockRange::from_block_number(15), BlockRange::new(15, 30));
        assert_eq!(BlockRange::from_block_number(16), BlockRange::new(15, 30));
        assert_eq!(BlockRange::from_block_number(29), BlockRange::new(15, 30));
    }

    #[test]
    fn test_block_range_from_number_and_length_with_valid_input() {
        assert_eq!(
            BlockRange::from_block_number_and_length(0, 10).unwrap(),
            BlockRange::new(0, 10)
        );
        assert_eq!(
            BlockRange::from_block_number_and_length(1, 10).unwrap(),
            BlockRange::new(0, 10)
        );
        assert_eq!(
            BlockRange::from_block_number_and_length(9, 10).unwrap(),
            BlockRange::new(0, 10)
        );
        assert_eq!(
            BlockRange::from_block_number_and_length(10, 10).unwrap(),
            BlockRange::new(10, 20)
        );
        assert_eq!(
            BlockRange::from_block_number_and_length(11, 10).unwrap(),
            BlockRange::new(10, 20)
        );
        assert_eq!(
            BlockRange::from_block_number_and_length(19, 10).unwrap(),
            BlockRange::new(10, 20)
        );
    }

    #[test]
    fn test_block_range_from_number_and_length_with_invalid_input() {
        BlockRange::from_block_number_and_length(10, 0)
            .expect_err("BlockRange should not be computed with a length of 0");
    }
}
