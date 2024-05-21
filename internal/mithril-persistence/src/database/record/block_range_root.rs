use sqlite::Row;

use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::BlockRange;

use crate::database::Hydrator;
use crate::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Block range root record is the representation of block range with its merkle root precomputed.
#[derive(Debug, PartialEq, Clone)]
pub struct BlockRangeRootRecord {
    /// Range of block numbers covered
    pub range: BlockRange,
    /// Merkle root of the block range, computed from the list of included transactions
    pub merkle_root: MKTreeNode,
}

impl From<(BlockRange, MKTreeNode)> for BlockRangeRootRecord {
    fn from(value: (BlockRange, MKTreeNode)) -> Self {
        Self {
            range: value.0,
            merkle_root: value.1,
        }
    }
}

impl From<BlockRangeRootRecord> for (BlockRange, MKTreeNode) {
    fn from(value: BlockRangeRootRecord) -> Self {
        (value.range, value.merkle_root)
    }
}

impl SqLiteEntity for BlockRangeRootRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let start = Hydrator::try_to_u64("block_range.start", row.read::<i64, _>(0))?;
        let end = Hydrator::try_to_u64("block_range.end", row.read::<i64, _>(1))?;
        let range = BlockRange::from_block_number(start);
        let merkle_root = row.read::<&str, _>(2);

        if range.start != start || range.end != end {
            return Err(HydrationError::InvalidData(format!(
                "Invalid block range: start={start}, end={end}, expected_start={}, expected_end={}",
                range.start, range.end
            )));
        }

        Ok(Self {
            range,
            merkle_root: MKTreeNode::from_hex(merkle_root)
                .map_err(|e| HydrationError::InvalidData(
                    format!(
                        "Field block_range.merkle_root (value={merkle_root}) is incompatible with hex representation. Error = {e}")
                )
                )?,
        })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            ("start", "{:block_range_root:}.start", "int"),
            ("end", "{:block_range_root:}.end", "int"),
            ("merkle_root", "{:block_range_root:}.merkle_root", "text"),
        ])
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Connection;

    use mithril_common::entities::BlockNumber;

    use super::*;

    fn select_block_range_from_db(start: BlockNumber, end: BlockNumber, merkle_root: &str) -> Row {
        let conn = Connection::open(":memory:").unwrap();
        let query = format!("SELECT {start}, {end}, '{merkle_root}'");
        let mut statement = conn.prepare(query).unwrap();
        statement.iter().next().unwrap().unwrap()
    }

    #[test]
    fn hydrate_succeed_if_valid_block_range_in_row() {
        // A valid block range has both bounds as multiples of block range length and the interval
        // size is equal to block range length.
        let row = select_block_range_from_db(0, BlockRange::LENGTH, "AAAA");
        let res = BlockRangeRootRecord::hydrate(row).expect("Expected hydrate to succeed");

        assert_eq!(
            res,
            BlockRangeRootRecord {
                range: BlockRange::from_block_number(0),
                merkle_root: MKTreeNode::from_hex("AAAA").unwrap(),
            }
        );
    }

    #[test]
    fn hydrate_fail_if_invalid_block_range_in_row() {
        for invalid_row in [
            // Start is not a multiple of block range length
            select_block_range_from_db(1, BlockRange::LENGTH, "AAAA"),
            // End is not a multiple of block range length
            select_block_range_from_db(0, BlockRange::LENGTH - 1, "AAAA"),
            // Interval is not equal to block range length
            select_block_range_from_db(0, BlockRange::LENGTH * 4, "AAAA"),
        ] {
            let res =
                BlockRangeRootRecord::hydrate(invalid_row).expect_err("Expected hydrate to fail");

            assert!(
                format!("{res:?}").contains("Invalid block range"),
                "Expected 'Invalid block range' error, got {:?}",
                res
            );
        }
    }
}
