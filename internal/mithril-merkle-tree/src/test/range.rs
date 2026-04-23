use anyhow::anyhow;
use serde::{Deserialize, Serialize};

use crate::{MKMapKey, MKTreeNode, StdResult};

/// `TEST ONLY` - A simple inclusive lower bound, exclusive upper bound range used as a [MKMapKey]
/// in tests and benchmarks.
#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Debug, Hash)]
pub struct TestRange {
    /// Inclusive lower bound of the range.
    pub start: u64,

    /// Exclusive upper bound of the range.
    pub end: u64,
}

impl TestRange {
    /// `TEST ONLY` - Build a new [TestRange] from its lower and upper bounds.
    pub fn new(start: u64, end: u64) -> Self {
        Self { start, end }
    }

    /// `TEST ONLY` - Combine two ranges into a new [TestRange] spanning both.
    ///
    /// Returns an error when the resulting range has invalid bounds.
    pub fn try_add(&self, other: &Self) -> StdResult<Self> {
        let start = self.start.min(other.start);
        let end = self.end.max(other.end);
        if start >= end {
            return Err(anyhow!(
                "TestRange could not add ranges with invalid bounds"
            ));
        }

        Ok(Self { start, end })
    }
}

impl PartialOrd for TestRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for TestRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.start.cmp(&other.start) {
            std::cmp::Ordering::Equal => self.end.cmp(&other.end),
            order => order,
        }
    }
}

impl From<TestRange> for MKTreeNode {
    fn from(value: TestRange) -> Self {
        let mut bytes = vec![];
        bytes.extend_from_slice(value.start.to_string().as_bytes());
        bytes.extend_from_slice(b"-");
        bytes.extend_from_slice(value.end.to_string().as_bytes());
        MKTreeNode::new(bytes)
    }
}

impl MKMapKey for TestRange {}
