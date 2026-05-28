//! Genesis-certificate cryptographic primitives.
//!
//! Groups the Ed25519 and SNARK-friendly (Schnorr over Jubjub) genesis
//! signers, the dual signing/verification-key bundles that pair them, and the wrapped
//! [`GenesisSigner`] that hides their parsing and selection behind a single type.

mod bundles;
mod ed25519;
#[cfg(feature = "future_snark")]
mod schnorr;
mod signer;

pub use bundles::*;
pub use ed25519::*;
#[cfg(feature = "future_snark")]
pub use schnorr::*;
pub use signer::*;
