mod delete_block_range_root;
mod get_block_range_root;
mod insert_block_range;

pub use delete_block_range_root::*;
pub use get_block_range_root::*;
pub use insert_block_range::*;

#[cfg(test)]
mod test_helper {
    use super::*;
    use crate::database::record::BlockRangeRootRecord;
    use crate::sqlite::{ConnectionExtensions, SqliteConnection};

    pub fn insert_block_range_roots(
        connection: &SqliteConnection,
        records: Vec<BlockRangeRootRecord>,
    ) {
        connection
            .fetch_first(InsertBlockRangeRootQuery::insert_many(records).unwrap())
            .unwrap();
    }
}
