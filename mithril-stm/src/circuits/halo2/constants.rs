//! Constants used by the Halo2 prototype.

pub use midnight_curves::EDWARDS_D;

use crate::circuits::halo2::types::JubjubBase;
pub(crate) use crate::signature_scheme::DST_SIGNATURE;

pub const DST_LOTTERY: JubjubBase = JubjubBase::from_raw([3, 3, 0, 0]);
