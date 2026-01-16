use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
};

use anyhow::{Context, Ok, anyhow};
use serde::{Deserialize, Serialize};

use crate::StmResult;

use super::{
    PrimeOrderProjectivePoint, ProjectivePoint, SchnorrSigningKey, UniqueSchnorrSignatureError,
};

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SchnorrVerificationKey(pub(crate) PrimeOrderProjectivePoint);

impl SchnorrVerificationKey {
    /// Convert a Schnorr signing key into a verification key
    ///
    /// This is done by computing `vk = g * sk` where g is the generator
    /// of the subgroup and sk is the schnorr signing key
    pub fn new_from_signing_key(signing_key: SchnorrSigningKey) -> StmResult<Self> {
        if signing_key.0.is_zero() | signing_key.0.is_one() {
            return Err(anyhow!(UniqueSchnorrSignatureError::InvalidSigningKey))
                .with_context(|| "Verification key generation failed.");
        }
        let generator = PrimeOrderProjectivePoint::create_generator();

        Ok(SchnorrVerificationKey(signing_key.0 * generator))
    }

    pub fn is_valid(&self) -> StmResult<Self> {
        let projective_point = ProjectivePoint::from(self.0);
        if !projective_point.is_prime_order() {
            return Err(anyhow!(UniqueSchnorrSignatureError::PointIsNotPrimeOrder(
                Box::new(self.0)
            )));
        }
        self.0.is_on_curve()?;

        Ok(*self)
    }

    /// Convert a `SchnorrVerificationKey` into bytes.
    pub fn to_bytes(self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert bytes into a `SchnorrVerificationKey`.
    ///
    /// The bytes must represent a Jubjub Subgroup point or the conversion will fail
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 32 {
            return Err(anyhow!(UniqueSchnorrSignatureError::Serialization)).with_context(
                || "Not enough bytes provided to construct a Schnorr verification key.",
            );
        }
        let prime_order_projective_point = PrimeOrderProjectivePoint::from_bytes(bytes)
            .with_context(|| "Cannot construct Schnorr verification key from given bytes.")?;

        Ok(SchnorrVerificationKey(prime_order_projective_point))
    }
}

impl Hash for SchnorrVerificationKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.to_bytes(), state)
    }
}

impl PartialOrd for SchnorrVerificationKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for SchnorrVerificationKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_bytes().cmp(&other.to_bytes())
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey};

    #[test]
    fn create_verification_key_from_signing_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();

        let vk = SchnorrVerificationKey::new_from_signing_key(sk);

        assert!(
            vk.is_ok(),
            "Verification key creation should succeed for valid signing key"
        );
    }

    #[test]
    fn different_signing_keys_produce_different_verification_keys() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk1 = SchnorrSigningKey::generate(&mut rng).unwrap();
        let sk2 = SchnorrSigningKey::generate(&mut rng).unwrap();

        let vk1 = SchnorrVerificationKey::new_from_signing_key(sk1).unwrap();
        let vk2 = SchnorrVerificationKey::new_from_signing_key(sk2).unwrap();

        assert_ne!(
            vk1, vk2,
            "Different signing keys should produce different verification keys"
        );
    }

    #[test]
    fn same_signing_key_produces_same_verification_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();

        let vk1 = SchnorrVerificationKey::new_from_signing_key(sk.clone()).unwrap();
        let vk2 = SchnorrVerificationKey::new_from_signing_key(sk).unwrap();

        assert_eq!(
            vk1, vk2,
            "Same signing key should produce same verification key"
        );
    }

    #[test]
    fn valid_verification_key_passes_validation() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk).unwrap();

        let result = vk.is_valid();

        assert!(
            result.is_ok(),
            "Valid verification key should pass validation"
        );
    }

    #[test]
    fn from_bytes_not_enough_bytes() {
        let bytes = vec![0u8; 31];
        let result = SchnorrVerificationKey::from_bytes(&bytes);

        result.expect_err("Should fail with insufficient bytes");
    }

    #[test]
    fn from_bytes_exact_size() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk).unwrap();
        let vk_bytes = vk.to_bytes();

        let vk_restored = SchnorrVerificationKey::from_bytes(&vk_bytes).unwrap();

        assert_eq!(vk, vk_restored);
    }

    #[test]
    fn from_bytes_extra_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk).unwrap();
        let vk_bytes = vk.to_bytes();

        let mut extended_bytes = vk_bytes.to_vec();
        extended_bytes.extend_from_slice(&[0xFF; 10]);

        let vk_restored = SchnorrVerificationKey::from_bytes(&extended_bytes).unwrap();

        assert_eq!(vk, vk_restored);
    }

    #[test]
    fn to_bytes_is_deterministic() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk).unwrap();

        let bytes1 = vk.to_bytes();
        let bytes2 = vk.to_bytes();

        assert_eq!(bytes1, bytes2, "to_bytes should be deterministic");
    }

    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 32] = &[
            144, 52, 95, 161, 127, 253, 49, 32, 140, 217, 231, 207, 32, 238, 244, 196, 97, 241, 47,
            95, 101, 9, 70, 136, 194, 66, 187, 253, 200, 32, 218, 43,
        ];

        fn golden_value() -> SchnorrVerificationKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            SchnorrVerificationKey::new_from_signing_key(sk).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = SchnorrVerificationKey::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = SchnorrVerificationKey::to_bytes(value);
            let golden_serialized = SchnorrVerificationKey::to_bytes(golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;

        const GOLDEN_JSON: &str = r#"[144, 52, 95, 161, 127, 253, 49, 32, 140, 217, 231, 207, 32, 238, 244, 196, 97, 241, 47, 95, 101, 9, 70, 136, 194, 66, 187, 253, 200, 32, 218, 43]"#;

        fn golden_value() -> SchnorrVerificationKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            SchnorrVerificationKey::new_from_signing_key(sk).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = serde_json::from_str(GOLDEN_JSON)
                .expect("This JSON deserialization should not fail");
            assert_eq!(golden_value(), value);

            let serialized =
                serde_json::to_string(&value).expect("This JSON serialization should not fail");
            let golden_serialized = serde_json::to_string(&golden_value())
                .expect("This JSON serialization should not fail");
            assert_eq!(golden_serialized, serialized);
        }
    }
}
