//! Shared database records

mod block_range_root;
mod cardano_transaction;
mod interval_without_block_range_root;

pub use block_range_root::*;
pub use cardano_transaction::*;
pub use interval_without_block_range_root::*;

// TODO: this probably should be in `mithril-persistence` crate
pub(crate) mod hydrator {
    use crate::sqlite::HydrationError;

    pub fn read_signed_entity_beacon_column<U: sqlite::RowIndex + Clone>(
        row: &sqlite::Row,
        column_index: U,
    ) -> String {
        // TODO: We need to check first that the cell can be read as a string first
        // (e.g. when beacon json is '{"network": "dev", "epoch": 1, "immutable_file_number": 2}').
        // If it fails, we fallback on reading the cell as an integer (e.g. when beacon json is '5').
        // Maybe there is a better way of doing this.
        match row.try_read::<&str, _>(column_index.clone()) {
            Ok(value) => value.to_string(),
            Err(_) => (row.read::<i64, _>(column_index)).to_string(),
        }
    }

    pub fn try_to_u64(field: &str, value: i64) -> Result<u64, HydrationError> {
        u64::try_from(value)
            .map_err(|e|
                HydrationError::InvalidData(
                    format!("Integer field {field} (value={value}) is incompatible with u64 representation. Error = {e}")
                )
            )
    }
}
