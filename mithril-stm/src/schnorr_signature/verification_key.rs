use anyhow::{Result, anyhow};
use group::{Group, GroupEncoding};
pub use midnight_curves::JubjubSubgroup;

use crate::schnorr_signature::signing_key::SchnorrSigningKey;

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct SchnorrVerificationKey(pub(crate) JubjubSubgroup);

impl SchnorrVerificationKey {
    /// TODO: Make sure this is correct as the previous implementation is
    /// using coordinates decomposition
    pub(crate) fn to_bytes(self) -> [u8; 32] {
        // let (x, y) = get_coordinates(self.0);
        // let mut bytes = [0u8; 64];
        // bytes[0..32].copy_from_slice(&x.to_bytes_le());
        // bytes[32..64].copy_from_slice(&y.to_bytes_le());
        // bytes
        self.0.to_bytes()
    }

    /// Do we really need to separate the coordinates?
    /// TODO: Make sure this is correct with some tests
    pub(crate) fn from_bytes(bytes: &[u8]) -> Result<Self> {
        let bytes = bytes
            .get(0..32)
            .ok_or(anyhow!("Not enough bytes to create a verification key."))?;

        let point = JubjubSubgroup::from_bytes(bytes.try_into()?)
            .into_option()
            .ok_or(anyhow!("Failed to create the JubjubSubGroup element."))?;

        Ok(SchnorrVerificationKey(point))
    }
}

impl From<&SchnorrSigningKey> for SchnorrVerificationKey {
    /// Convert a Shnorr secret key into a verification key
    /// This is done by computing `vk = g * sk` where g is the generator
    /// of the subgroup and sk is the schnorr secret key
    fn from(sk: &SchnorrSigningKey) -> Self {
        let g = JubjubSubgroup::generator();
        SchnorrVerificationKey(g * sk.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn test_generate_signing_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let _vk = SchnorrVerificationKey::from(&sk);
    }
}
