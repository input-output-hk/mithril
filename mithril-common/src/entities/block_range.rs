use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result},
    ops::{Deref, Range, RangeInclusive},
};

use anyhow::anyhow;
use serde::{Deserialize, Serialize};

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

    /// Get the start of the block range that contains the given block number
    pub fn start(number: BlockNumber) -> BlockNumber {
        Self::start_with_length(number, Self::LENGTH)
    }

    /// Get all [BlockRange] strictly contained in the given interval
    pub fn all_block_ranges_in(interval: RangeInclusive<BlockNumber>) -> BlockRangesSequence {
        BlockRangesSequence::new(interval)
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
        let block_range_start = Self::start_with_length(number, length);
        let block_range_end = block_range_start + length;
        Ok(Self {
            inner_range: block_range_start..block_range_end,
        })
    }

    /// Get the start of the block range of given length that contains the given block number
    fn start_with_length(number: BlockNumber, length: BlockRangeLength) -> BlockNumber {
        // the formula used to compute the lower bound of the block range is `⌊number / length⌋ * length`
        // the computation of the floor is done with the integer division `/` of rust
        (number / length) * length
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

/// A continuous iterable sequence of [block ranges][BlockRange].
///
/// Yielded block ranges are sized by [BlockRange::LENGTH], and always have
/// bounds that are multiples of [BlockRange::LENGTH].
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BlockRangesSequence {
    start: BlockNumber,
    end: BlockNumber,
}

impl BlockRangesSequence {
    /// Build the [BlockRangesSequence] strictly contained in the given interval.
    ///
    /// The interval bounds will be corrected to be multiples of [BlockRange::LENGTH].
    pub fn new(interval: RangeInclusive<BlockNumber>) -> Self {
        let start = if (interval.start() % BlockRange::LENGTH) == 0 {
            *interval.start()
        } else {
            BlockRange::start(*interval.start()) + BlockRange::LENGTH
        };
        // End is inclusive, so we need to add 1
        let end = BlockRange::start(*interval.end() + 1);

        Self { start, end }
    }

    /// Returns the start of the block ranges sequence.
    pub fn start(&self) -> BlockNumber {
        self.start
    }

    /// Returns the end of the block ranges sequence.
    pub fn end(&self) -> BlockNumber {
        self.end
    }

    /// Returns `true` if range is contained in the sequence.
    pub fn contains(&self, range: &Range<BlockNumber>) -> bool {
        self.start <= range.start && range.end <= self.end
    }

    /// Returns `true` if the block ranges sequence contains no elements.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Consume `self` into a new Vec
    pub fn into_vec(self) -> Vec<BlockRange> {
        self.into_iter().collect()
    }
}

impl Iterator for BlockRangesSequence {
    type Item = BlockRange;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            return None;
        }

        let block_range = BlockRange::from_block_number(self.start);
        self.start = block_range.end;
        Some(block_range)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.start as usize, Some(self.end as usize))
    }
}

impl ExactSizeIterator for BlockRangesSequence {
    fn len(&self) -> usize {
        ((self.end - self.start) / BlockRange::LENGTH) as usize
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Not;

    use super::*;

    #[test]
    fn test_block_range_contains() {
        let block_range = BlockRange::new(1, 10);

        assert!(block_range.contains(&1));
        assert!(block_range.contains(&6));
        assert!(block_range.contains(&9));

        assert!(block_range.contains(&0).not());
        // The end of the range is exclusive
        assert!(block_range.contains(&10).not());
    }

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
    fn test_block_range_start() {
        assert_eq!(BlockRange::start(0), 0);
        assert_eq!(BlockRange::start(1), 0);
        assert_eq!(BlockRange::start(14), 0);
        assert_eq!(BlockRange::start(15), 15);
        assert_eq!(BlockRange::start(16), 15);
        assert_eq!(BlockRange::start(29), 15);
    }

    #[test]
    fn test_block_range_all_block_ranges_in() {
        assert_eq!(BlockRange::all_block_ranges_in(0..=0).into_vec(), vec![]);
        assert_eq!(BlockRange::all_block_ranges_in(0..=1).into_vec(), vec![]);
        assert_eq!(BlockRange::all_block_ranges_in(0..=13).into_vec(), vec![]);
        assert_eq!(BlockRange::all_block_ranges_in(1..=14).into_vec(), vec![]);
        assert_eq!(
            BlockRange::all_block_ranges_in(0..=14).into_vec(),
            vec![BlockRange::new(0, 15)]
        );
        assert_eq!(
            BlockRange::all_block_ranges_in(0..=15).into_vec(),
            vec![BlockRange::new(0, 15)]
        );
        assert_eq!(
            BlockRange::all_block_ranges_in(14..=29).into_vec(),
            vec![BlockRange::new(15, 30)]
        );
        assert_eq!(
            BlockRange::all_block_ranges_in(14..=30).into_vec(),
            vec![BlockRange::new(15, 30)]
        );
        assert_eq!(
            BlockRange::all_block_ranges_in(14..=60).into_vec(),
            vec![
                BlockRange::new(15, 30),
                BlockRange::new(30, 45),
                BlockRange::new(45, 60)
            ]
        );
    }

    #[test]
    fn test_block_ranges_sequence_is_empty() {
        assert!(BlockRange::all_block_ranges_in(0..=0).is_empty());
        assert!(BlockRange::all_block_ranges_in(0..=1).is_empty());
        assert!(BlockRange::all_block_ranges_in(0..=13).is_empty());
        assert!(BlockRange::all_block_ranges_in(1..=14).is_empty());
        assert!(BlockRange::all_block_ranges_in(0..=14).is_empty().not());
        assert!(BlockRange::all_block_ranges_in(0..=15).is_empty().not());
        assert!(BlockRange::all_block_ranges_in(14..=29).is_empty().not());
        assert!(BlockRange::all_block_ranges_in(14..=30).is_empty().not());
        assert!(BlockRange::all_block_ranges_in(14..=60).is_empty().not());
    }

    #[test]
    fn test_block_ranges_sequence_len() {
        assert_eq!(
            BlockRange::all_block_ranges_in(0..=(BlockRange::LENGTH - 2)).len(),
            0
        );
        assert_eq!(
            BlockRange::all_block_ranges_in(0..=(BlockRange::LENGTH - 1)).len(),
            1
        );
        assert_eq!(
            BlockRange::all_block_ranges_in(0..=(BlockRange::LENGTH * 15)).len(),
            15
        );
    }

    #[test]
    fn test_block_ranges_sequence_contains() {
        let block_range = BlockRange::new(15, 30);
        assert!(BlockRange::all_block_ranges_in(0..=14)
            .contains(&block_range)
            .not());
        assert!(BlockRange::all_block_ranges_in(30..=59)
            .contains(&block_range)
            .not());
        assert!(BlockRange::all_block_ranges_in(0..=29).contains(&block_range));
        assert!(BlockRange::all_block_ranges_in(15..=29).contains(&block_range));
        assert!(BlockRange::all_block_ranges_in(15..=44).contains(&block_range));
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
