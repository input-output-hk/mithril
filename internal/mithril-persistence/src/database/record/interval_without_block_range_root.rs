use std::ops::Range;

use anyhow::anyhow;
use sqlite::Row;

use mithril_common::entities::BlockNumber;
use mithril_common::StdResult;

use crate::database::Hydrator;
use crate::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Interval of block numbers (`[start, end[`) without block ranges root.
pub struct IntervalWithoutBlockRangeRootRecord {
    /// Start of the interval, fetched from the last block range root end
    pub start: Option<BlockNumber>,
    /// End of the interval, fetched from the last transaction block number
    pub end: Option<BlockNumber>,
}

impl IntervalWithoutBlockRangeRootRecord {
    /// Deduce from self the range of blocks that are available to compute block range roots.
    pub fn to_range(&self) -> StdResult<Option<Range<BlockNumber>>> {
        match (self.start, self.end) {
            // No transactions stored - nothing can be computed
            (_, None) => Ok(None),
            // Transactions stored but no block range root - everything can be computed
            (None, Some(end)) => Ok(Some(0..(end + 1))),
            // Last stored transactions is at the end of the last block range - nothing to compute
            (Some(start), Some(end)) if (end + 1) == start => Ok(None),
            (Some(start), Some(end)) if end < start => {
                Err(anyhow!(
                        "Inconsistent state: \
                        Last block range root block number ({start}) is higher than the last transaction block number ({end}). \
                        Did you forgot to prune obsolete `block_range_root` after a transaction rollback ?"
                    ))
            }
            // Nominal case
            (Some(start), Some(end)) => Ok(Some(start..(end + 1))),
        }
    }
}

impl SqLiteEntity for IntervalWithoutBlockRangeRootRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let start = row
            .read::<Option<i64>, _>(0)
            .map(|v| Hydrator::try_to_u64("interval_start.start", v))
            .transpose()?;
        let end = row
            .read::<Option<i64>, _>(1)
            .map(|v| Hydrator::try_to_u64("interval_end.end", v))
            .transpose()?;

        Ok(Self { start, end })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            ("start", "{:interval_start:}.start", "int"),
            ("end", "{:interval_end:}.end", "int"),
        ])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn db_without_block_range_nor_transactions_yield_none() {
        let range = IntervalWithoutBlockRangeRootRecord {
            start: None,
            end: None,
        }
        .to_range()
        .unwrap();
        assert_eq!(None, range);
    }

    #[test]
    fn db_with_only_transactions_yield_a_range() {
        let range = IntervalWithoutBlockRangeRootRecord {
            start: None,
            end: Some(10),
        }
        .to_range()
        .unwrap();
        assert_eq!(Some(0..11), range);
    }

    #[test]
    fn db_with_only_block_range_and_no_transactions_yield_none() {
        let range = IntervalWithoutBlockRangeRootRecord {
            start: Some(10),
            end: None,
        }
        .to_range()
        .unwrap();
        assert_eq!(None, range);
    }

    #[test]
    fn db_with_both_block_range_and_transactions_yield_a_range() {
        let range = IntervalWithoutBlockRangeRootRecord {
            start: Some(2),
            end: Some(10),
        }
        .to_range()
        .unwrap();
        assert_eq!(Some(2..11), range);
    }

    #[test]
    fn db_with_last_transaction_block_number_right_at_the_end_of_the_last_block_range_yield_none() {
        // Reminder: the end of the block range is exclusive
        let range = IntervalWithoutBlockRangeRootRecord {
            start: Some(10),
            end: Some(9),
        }
        .to_range()
        .unwrap();
        assert_eq!(None, range);
    }

    #[test]
    fn db_with_last_transaction_block_number_below_the_end_of_the_last_block_range_yield_an_error()
    {
        // Reminder: the end of the block range is exclusive
        let res = IntervalWithoutBlockRangeRootRecord {
            start: Some(10),
            end: Some(8),
        }
        .to_range()
        .unwrap_err();
        assert!(
            format!("{res:?}").contains("Inconsistent state"),
            "Expected 'Inconsistent state' error, got {res:?}",
        );
    }
}
