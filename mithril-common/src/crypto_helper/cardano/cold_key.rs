use ed25519_dalek::Keypair as ColdKeypair;
use rand_chacha_dalek_compat::rand_core::SeedableRng;
use rand_chacha_dalek_compat::ChaCha20Rng;

/// A cold key generator / test only
#[derive(Debug)]
pub struct ColdKeyGenerator();

impl ColdKeyGenerator {
    pub(crate) fn create_deterministic_keypair(seed: [u8; 32]) -> ColdKeypair {
        let mut rng = ChaCha20Rng::from_seed(seed);
        ColdKeypair::generate(&mut rng)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_deterministic_genesis_keypair() {
        let cold_keypair1 = ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
        let cold_keypair2 = ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
        let cold_keypair3 = ColdKeyGenerator::create_deterministic_keypair([1u8; 32]);
        assert_eq!(cold_keypair1.to_bytes(), cold_keypair2.to_bytes());
        assert_ne!(cold_keypair1.to_bytes(), cold_keypair3.to_bytes());
    }
}
