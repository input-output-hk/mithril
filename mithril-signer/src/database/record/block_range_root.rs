use sqlite::Row;

use mithril_common::crypto_helper::MKTreeNode;
use mithril_common::entities::BlockRange;
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

use crate::database::record::hydrator::try_to_u64;

/// Block range root record is the representation of block range with its merkle root precomputed.
#[derive(Debug, PartialEq, Clone)]
pub struct BlockRangeRootRecord {
    /// Range of block numbers covered
    pub range: BlockRange,
    /// Merkle root of the block range, computed from the list of all transactions that are
    /// included in the range
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
        let start = try_to_u64("block_range.start", row.read::<i64, _>(0))?;
        let _end = try_to_u64("block_range.end", row.read::<i64, _>(1))?;
        let merkle_root = row.read::<&str, _>(2);

        Ok(Self {
            range: BlockRange::from_block_number(start),
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
