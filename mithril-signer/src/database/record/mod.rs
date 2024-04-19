//! Signer related database records

mod block_range_root;
mod cardano_transaction;
mod interval_without_block_range_root;

pub use block_range_root::*;
pub use cardano_transaction::*;
pub use interval_without_block_range_root::*;

// TODO: this probably should be in `mithril-persistence` crate
pub(crate) mod hydrator {
    use mithril_persistence::sqlite::HydrationError;

    pub fn try_to_u64(field: &str, value: i64) -> Result<u64, HydrationError> {
        u64::try_from(value)
            .map_err(|e|
                HydrationError::InvalidData(
                    format!("Integer field {field} (value={value}) is incompatible with u64 representation. Error = {e}")
                )
            )
    }
}
