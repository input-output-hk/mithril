mod error;
mod signature;
mod signing_key;
mod utils;
mod verification_key;

pub use error::*;
pub use signature::*;
pub use signing_key::*;
pub(crate) use utils::*;
pub use verification_key::*;

use dusk_jubjub::Fq as JubjubBase;

/// A DST (Domain Separation Tag) to distinguish between use of Poseidon hash
const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([0u64, 0, 0, 0]);
