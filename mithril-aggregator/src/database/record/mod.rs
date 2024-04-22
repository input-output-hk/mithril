//! Aggregator related database records

mod block_range_root;
mod cardano_transaction;
mod certificate;
mod epoch_setting;
mod interval_without_block_range_root;
mod open_message;
mod open_message_with_single_signatures;
mod signed_entity;
mod signer;
mod signer_registration;
mod single_signature;
mod stake_pool;

pub use block_range_root::*;
pub use cardano_transaction::*;
pub use certificate::*;
pub use epoch_setting::*;
pub use interval_without_block_range_root::*;
pub use open_message::*;
pub use open_message_with_single_signatures::*;
pub use signed_entity::*;
pub use signer::*;
pub use signer_registration::*;
pub use single_signature::*;
pub use stake_pool::*;

// TODO: this probably should be in `mithril-persistence` crate
pub(crate) mod hydrator {
    use mithril_persistence::sqlite::HydrationError;

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
