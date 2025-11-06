use ff::Field;
use midnight_curves::{Fr as JubjubScalar, JubjubSubgroup};
use rand_core::{CryptoRng, RngCore};

use group::Group;

use crate::schnorr_signature::signature::SchnorrSignature;

pub(crate) struct SchnorrSigningKey(pub(crate) JubjubScalar);

impl SchnorrSigningKey {
    pub(crate) fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        SchnorrSigningKey(JubjubScalar::random(rng))
    }

    fn sign(&self, msg: &[u8], rng: &mut (impl RngCore + CryptoRng)) -> SchnorrSignature {
        // Use the subgroup generator to compute the curve points
        let g = JubjubSubgroup::generator();
        let vk = todo!();

        // SchnorrSignature {
        //     sigma: 0,
        //     s: 0,
        //     c: 0,
        // }
        todo!()
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
        let _sk = SchnorrSigningKey::generate(&mut rng);
    }
}
