use ed25519_dalek::{ExpandedSecretKey, PublicKey as ColdPublicKey, SecretKey as ColdSecretKey};
use rand_chacha_dalek_compat::rand_core::{self, CryptoRng, RngCore, SeedableRng};
use rand_chacha_dalek_compat::ChaCha20Rng;

/// A cold key generator / test only
#[derive(Debug)]
pub struct ColdKeyGenerator();

impl ColdKeyGenerator {
    /// Create keypair from random generator
    fn create_keypair<R>(mut rng: R) -> (ColdSecretKey, ColdPublicKey)
    where
        R: CryptoRng + RngCore,
    {
        let cold_secret_key = ColdSecretKey::generate(&mut rng);
        let cold_secret_key_expanded = ExpandedSecretKey::from(&cold_secret_key);
        let cold_verification_key: ColdPublicKey = (&cold_secret_key_expanded).into();
        (cold_secret_key, cold_verification_key)
    }

    pub(crate) fn create_deterministic_keypair(seed: [u8; 32]) -> (ColdSecretKey, ColdPublicKey) {
        let rng = ChaCha20Rng::from_seed(seed);
        Self::create_keypair(rng)
    }

    #[allow(dead_code)]
    pub(crate) fn create_non_deterministic_keypair() -> (ColdSecretKey, ColdPublicKey) {
        let rng = rand_core::OsRng;
        Self::create_keypair(rng)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_deterministic_genesis_keypair() {
        let (cold_secret_key1, cold_verification_key1) =
            ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
        let (cold_secret_key2, cold_verification_key2) =
            ColdKeyGenerator::create_deterministic_keypair([0u8; 32]);
        let (cold_secret_key3, cold_verification_key3) =
            ColdKeyGenerator::create_deterministic_keypair([1u8; 32]);
        assert_eq!(cold_secret_key1.as_bytes(), cold_secret_key2.as_bytes());
        assert_ne!(cold_secret_key1.as_bytes(), cold_secret_key3.as_bytes());
        assert_eq!(
            cold_verification_key1.as_bytes(),
            cold_verification_key2.as_bytes()
        );
        assert_ne!(
            cold_verification_key1.as_bytes(),
            cold_verification_key3.as_bytes()
        );
    }

    #[test]
    fn test_generate_non_deterministic_genesis_keypair() {
        let (cold_secret_key1, cold_verification_key1) =
            ColdKeyGenerator::create_non_deterministic_keypair();
        let (cold_secret_key2, cold_verification_key2) =
            ColdKeyGenerator::create_non_deterministic_keypair();

        assert_ne!(cold_secret_key1.as_bytes(), cold_secret_key2.as_bytes());
        assert_ne!(
            cold_verification_key1.as_bytes(),
            cold_verification_key2.as_bytes()
        );
    }
}
