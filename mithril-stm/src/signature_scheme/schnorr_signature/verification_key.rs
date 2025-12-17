use anyhow::{Context, Ok, anyhow};
use serde::{Deserialize, Serialize};

use super::{PrimeOrderProjectivePoint, ProjectivePoint, SchnorrSignatureError, SchnorrSigningKey};
use crate::StmResult;

/// Schnorr verification key, it consists of a point on the Jubjub curve
/// vk = g * sk, where g is a generator
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct SchnorrVerificationKey(pub(crate) PrimeOrderProjectivePoint);

impl SchnorrVerificationKey {
    /// Convert a Schnorr secret key into a verification key
    ///
    /// This is done by computing `vk = g * sk` where g is the generator
    /// of the subgroup and sk is the schnorr secret key
    pub fn new_from_signing_key(signing_key: SchnorrSigningKey) -> StmResult<Self> {
        if signing_key.0.is_zero() | signing_key.0.is_one() {
            return Err(anyhow!(SchnorrSignatureError::InvalidSigningKey))
                .with_context(|| "Verification key generation failed.");
        }
        let generator = PrimeOrderProjectivePoint::create_generator();

        Ok(SchnorrVerificationKey(
            generator.scalar_multiplication(&signing_key.0),
        ))
    }

    pub fn is_valid(&self) -> StmResult<Self> {
        let projective_point = ProjectivePoint::from_prime_order_projective_point(self.0);
        if !projective_point.is_prime_order() {
            return Err(anyhow!(SchnorrSignatureError::PointIsNotPrimeOrder(
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
            return Err(anyhow!(SchnorrSignatureError::SerializationError)).with_context(
                || "Not enough bytes provided to construct a Schnorr verification key.",
            );
        }
        let prime_order_projective_point = PrimeOrderProjectivePoint::from_bytes(bytes)
            .with_context(|| "Cannot construct Schnorr verification key from given bytes.")?;

        Ok(SchnorrVerificationKey(prime_order_projective_point))
    }
}

#[cfg(test)]
mod tests {
    mod golden {

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::signature_scheme::{SchnorrSigningKey, SchnorrVerificationKey};

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
