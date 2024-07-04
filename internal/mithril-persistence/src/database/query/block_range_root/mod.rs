mod delete_block_range_root;
mod get_block_range_root;
mod get_interval_without_block_range;
mod insert_block_range;

pub use delete_block_range_root::*;
pub use get_block_range_root::*;
pub use get_interval_without_block_range::*;
pub use insert_block_range::*;

#[cfg(test)]
mod test_helper {
    use crate::database::record::BlockRangeRootRecord;
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    use super::*;

    pub fn insert_block_range_roots(
        connection: &SqliteConnection,
        records: Vec<BlockRangeRootRecord>,
    ) {
        connection
            .fetch_first(InsertBlockRangeRootQuery::insert_many(records).unwrap())
            .unwrap();
    }
}
