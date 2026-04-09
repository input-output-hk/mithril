//! Constraint gadgets used by the Halo2 STM circuit.
//! Domain-oriented gadgets are re-exported here; low-level comparison helpers stay internal.

mod comparison;
mod comparison_helpers;
mod lottery;
mod merkle_path;
mod unique_schnorr_signature;

/// Number of bits used for lottery index comparisons in the circuit.
///
/// This bounds both `m` and all lottery indices to fit in 16 bits.
pub(crate) const LOTTERY_INDEX_BITS: u32 = 16;

pub(crate) use lottery::{
    assert_lottery_index_in_bounds, assert_lottery_won, assert_strictly_increasing_lottery_index,
};
pub(crate) use merkle_path::{MerklePathInputs, verify_merkle_path};
pub(crate) use unique_schnorr_signature::{UniqueSchnorrSignatureInputs, verify_unique_signature};
