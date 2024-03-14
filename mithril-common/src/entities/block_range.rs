use anyhow::anyhow;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    ops::{Deref, Range},
};

use crate::{
    crypto_helper::{MKMapKey, MKTreeNode},
    StdResult,
};

/// BlockNumber is the block number of a Cardano transaction.
pub type BlockNumber = u64;

/// BlockRange for the Cardano chain
#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Debug, Hash)]
pub struct BlockRange {
    inner_range: Range<u64>,
}

impl BlockRange {
    /// BlockRange factory
    pub fn new(start: BlockNumber, end: BlockNumber) -> Self {
        Self {
            inner_range: start..end,
        }
    }

    /// Try to add two BlockRanges
    pub fn try_add(&mut self, other: &BlockRange) -> StdResult<BlockRange> {
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
        let range_1 = BlockRange::new(1, 10);
        let range_2 = BlockRange::new(1, 10);
        let range_3 = BlockRange::new(1, 11);
        let range_4 = BlockRange::new(2, 10);

        assert_eq!(range_1, range_2);
        assert_ne!(range_1, range_3);
        assert_ne!(range_1, range_4);
        assert_ne!(range_3, range_4);

        assert!(range_1 < range_3);
        assert!(range_1 < range_4);
        assert!(range_3 < range_4);
    }

    #[test]
    fn test_block_range_try_add() {
        let mut range_1 = BlockRange::new(1, 10);
        let range_2 = BlockRange::new(1, 10);
        let range_3 = BlockRange::new(1, 11);
        let range_4 = BlockRange::new(2, 10);

        assert_eq!(range_1.try_add(&range_2).unwrap(), range_1);
        assert_eq!(range_1.try_add(&range_3).unwrap(), range_3);
        assert_eq!(range_1.try_add(&range_4).unwrap(), BlockRange::new(1, 10));
    }
}
