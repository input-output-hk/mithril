//! Constraint gadgets used by the Halo2 STM circuit.
//! Domain-oriented gadgets are re-exported here; low-level comparison helpers stay internal.

mod merkle;
mod unique_schnorr_signature;
mod lottery;
mod comparison;

pub(crate) use merkle::verify_merkle_path;
pub(crate) use unique_schnorr_signature::verify_unique_signature;
pub(crate) use lottery::{
    assert_lottery_index_in_bounds, assert_lottery_won, assert_strictly_increasing_lottery_index,
};
