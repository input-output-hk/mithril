use ff::Field;
use midnight_curves::Fr as JubjubScalar;
use rand_core::{CryptoRng, RngCore};

struct SchnorrSigningKey(JubjubScalar);

impl SchnorrSigningKey {
    fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        SchnorrSigningKey(JubjubScalar::random(rng))
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
