use sqlite::Row;

use mithril_common::entities::{BlockHash, BlockNumber, CardanoBlockWithTransactions, SlotNumber};

use crate::database::Hydrator;
use crate::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Cardano block record is the representation of a cardano block stored in the sqlite database.
#[derive(Debug, PartialEq, Clone)]
pub struct CardanoBlockRecord {
    /// Hash of the block
    pub block_hash: BlockHash,

    /// Number of the block
    pub block_number: BlockNumber,

    /// Slot number of the block
    pub slot_number: SlotNumber,
}

impl CardanoBlockRecord {
    /// SQLite max variables per prepared query is `32 766`, given each record needs to binds three variables and to leave some
    /// room for other variables (i.e. in WHERE clause) we fix this limit to 10k, meaning 30 000 binds at once maximum
    pub const MAX_PER_INSERT: usize = 10_000;

    /// CardanoBlockRecord factory
    pub fn new<T: Into<BlockHash>>(
        block_hash: T,
        block_number: BlockNumber,
        slot_number: SlotNumber,
    ) -> Self {
        Self {
            block_hash: block_hash.into(),
            block_number,
            slot_number,
        }
    }
}

impl SqLiteEntity for CardanoBlockRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let block_hash = row.read::<&str, _>(0);
        let block_number =
            Hydrator::try_to_u64("cardano_block.block_number", row.read::<i64, _>(1))?;
        let slot_number = Hydrator::try_to_u64("cardano_block.slot_number", row.read::<i64, _>(2))?;

        Ok(Self {
            block_hash: block_hash.to_string(),
            block_number: BlockNumber(block_number),
            slot_number: SlotNumber(slot_number),
        })
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            ("block_hash", "{:cardano_block:}.block_hash", "text"),
            ("block_number", "{:cardano_block:}.block_number", "int"),
            ("slot_number", "{:cardano_block:}.slot_number", "int"),
        ])
    }
}

impl From<CardanoBlockWithTransactions> for CardanoBlockRecord {
    fn from(block: CardanoBlockWithTransactions) -> Self {
        Self::new(block.block_hash, block.block_number, block.slot_number)
    }
}
