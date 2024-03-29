//! Aggregator related database providers
mod cardano_transaction;
mod certificate;
mod epoch_setting;
mod open_message;
mod signed_entity;
mod signer;
mod signer_registration;
mod single_signature;
mod stake_pool;
#[cfg(test)]
mod test_helper;

pub use cardano_transaction::*;
pub use certificate::*;
pub use epoch_setting::*;
pub use open_message::*;
pub use signed_entity::*;
pub use signer::*;
pub use signer_registration::*;
pub use single_signature::*;
pub use stake_pool::*;
#[cfg(test)]
pub use test_helper::*;

pub(crate) fn read_signed_entity_beacon_column<U: sqlite::RowIndex + Clone>(
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
