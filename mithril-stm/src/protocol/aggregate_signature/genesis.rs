//! Genesis verification keys carried into aggregate signature verification.

#[cfg(feature = "future_snark")]
use crate::SchnorrVerificationKey;

/// Genesis verification keys the aggregate signature verifier needs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenesisVerificationKeyBundle {
    /// Schnorr-genesis verification key (Schnorr over Jubjub).
    #[cfg(feature = "future_snark")]
    pub schnorr: SchnorrVerificationKey,
}

impl GenesisVerificationKeyBundle {
    /// Build a fresh bundle from the genesis Schnorr verification key.
    #[cfg_attr(not(feature = "future_snark"), allow(clippy::new_without_default))]
    pub fn new(#[cfg(feature = "future_snark")] schnorr: SchnorrVerificationKey) -> Self {
        Self {
            #[cfg(feature = "future_snark")]
            schnorr,
        }
    }
}
