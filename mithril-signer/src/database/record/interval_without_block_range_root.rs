use sqlite::Row;

use mithril_common::entities::BlockNumber;
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

use crate::database::record::hydrator::try_to_u64;

/// Interval of block numbers without block ranges root.
pub struct IntervalWithoutBlockRangeRootRecord {
    /// Start of the interval
    pub start: Option<BlockNumber>,
    /// End of the interval
    pub end: Option<BlockNumber>,
}

impl SqLiteEntity for IntervalWithoutBlockRangeRootRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let start = row
            .read::<Option<i64>, _>(0)
            .map(|v| try_to_u64("interval_start.start", v))
            .transpose()?;
        let end = row
            .read::<Option<i64>, _>(1)
            .map(|v| try_to_u64("interval_end.end", v))
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
