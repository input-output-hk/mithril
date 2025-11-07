use group::Group;
pub use midnight_curves::JubjubSubgroup;

use crate::schnorr_signature::signing_key::SchnorrSigningKey;

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default)]
pub struct SchnorrVerificationKey(pub(crate) JubjubSubgroup);

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
