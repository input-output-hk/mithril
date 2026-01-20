//! Constants used by the Halo2 prototype.

use crate::circuits::halo2::types::JubjubBase;

pub use midnight_curves::EDWARDS_D;

/// Domain separation tag for the unique Schnorr signature.
/// Mirrors the value used in `mithril-circuits` (prototype stage).
pub const DST_UNIQUE_SIGNATURE: JubjubBase = JubjubBase::from_raw([2, 2, 0, 0]);
pub const DST_LOTTERY: JubjubBase = JubjubBase::from_raw([3, 3, 0, 0]);
