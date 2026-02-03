use anyhow::{Context, anyhow};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use crate::StmResult;

use super::{
    BaseFieldElement, PrimeOrderProjectivePoint, ProjectivePoint, ScalarFieldElement,
    SchnorrVerificationKey, UniqueSchnorrSignature, UniqueSchnorrSignatureError,
    compute_poseidon_digest,
};

/// Schnorr Signing key, it is essentially a random scalar of the Jubjub scalar field
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct SchnorrSigningKey(pub(crate) ScalarFieldElement);

impl SchnorrSigningKey {
    /// Generate a random scalar value to use as signing key
    pub fn generate<R: RngCore + CryptoRng>(rng: &mut R) -> Self {
        SchnorrSigningKey(ScalarFieldElement::new_random_scalar(rng))
    }

    /// This function is an adapted version of the Schnorr signature scheme that includes
    /// the computation of a deterministic value (called commitment_point) based on the message and the signing key
    /// and works with the Jubjub elliptic curve and the Poseidon hash function.
    ///
    /// Input:
    ///     - a message: some bytes
    ///     - a signing key: an element of the scalar field of the Jubjub curve
    /// Output:
    ///     - a unique signature of the form (commitment_point, response, challenge):
    ///         - commitment_point is deterministic depending only on the message and signing key
    ///         - the response and challenge depends on a random value generated during the signature
    ///
    /// The protocol computes:
    ///     - commitment_point = signing_key * H(Sha256(msg))
    ///     - random_scalar, a random value
    ///     - random_point_1 = random_scalar * H(Sha256(msg))
    ///     - random_point_2 = random_scalar * prime_order_generator_point, where generator is a generator of the prime-order subgroup of Jubjub
    ///     - challenge = Poseidon(DST || H(Sha256(msg)) || verification_key || commitment_point || random_point_1 || random_point_2)
    ///     - response = random_scalar - challenge * signing_key
    ///
    /// Output the signature (`commitment_point`, `response`, `challenge`)
    ///
    pub fn sign<R: RngCore + CryptoRng>(
        &self,
        msg: &[u8],
        rng: &mut R,
    ) -> StmResult<UniqueSchnorrSignature> {
        // Use the subgroup generator to compute the curve points
        let prime_order_generator_point = PrimeOrderProjectivePoint::create_generator();
        let verification_key = SchnorrVerificationKey::new_from_signing_key(self.clone())
            .with_context(|| "Could not generate verification key from signing key.")?;

        // First hashing the message to a scalar then hashing it to a curve point
        let msg_hash_point = ProjectivePoint::hash_to_projective_point(msg)?;

        let commitment_point = self.0 * msg_hash_point;

        let random_scalar = ScalarFieldElement::new_random_nonzero_scalar(rng)
            .with_context(|| "Random scalar generation failed during signing.")?;

        let random_point_1 = random_scalar * msg_hash_point;
        let random_point_2 = random_scalar * prime_order_generator_point;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        // The order must be preserved
        let points_coordinates: Vec<BaseFieldElement> = [
            msg_hash_point,
            ProjectivePoint::from(verification_key.0),
            commitment_point,
            random_point_1,
            ProjectivePoint::from(random_point_2),
        ]
        .iter()
        .flat_map(|point| {
            let (u, v) = point.get_coordinates();
            [u, v]
        })
        .collect();

        let challenge = compute_poseidon_digest(&points_coordinates);
        let challenge_times_sk = ScalarFieldElement::from_base_field(&challenge)? * self.0;
        let response = random_scalar - challenge_times_sk;

        Ok(UniqueSchnorrSignature {
            commitment_point,
            response,
            challenge,
        })
    }

    /// Convert a `SchnorrSigningKey` into bytes.
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert bytes into a `SchnorrSigningKey`.
    ///
    /// The bytes must represent a Jubjub scalar or the conversion will fail
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 32 {
            return Err(anyhow!(UniqueSchnorrSignatureError::Serialization)).with_context(
                || "Not enough bytes provided to re-construct a Schnorr signing key.",
            );
        }
        let scalar_field_element = ScalarFieldElement::from_bytes(bytes)
            .with_context(|| "Could not construct Schnorr signing key from given bytes.")?;
        Ok(SchnorrSigningKey(scalar_field_element))
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::SchnorrSigningKey;

    #[test]
    fn generate_different_keys() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk1 = SchnorrSigningKey::generate(&mut rng);
        let sk2 = SchnorrSigningKey::generate(&mut rng);

        // Keys should be different
        assert_ne!(sk1, sk2, "Different keys should be generated");
    }

    #[test]
    fn from_bytes_not_enough_bytes() {
        let bytes = vec![0u8; 31];
        let result = SchnorrSigningKey::from_bytes(&bytes);

        result.expect_err("Should fail with insufficient bytes");
    }

    #[test]
    fn from_bytes_exact_size() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let sk_bytes = sk.to_bytes();

        let sk_restored = SchnorrSigningKey::from_bytes(&sk_bytes).unwrap();

        assert_eq!(sk, sk_restored);
    }

    #[test]
    fn from_bytes_extra_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let sk_bytes = sk.to_bytes();

        let mut extended_bytes = sk_bytes.to_vec();
        extended_bytes.extend_from_slice(&[0xFF; 10]);

        let sk_restored = SchnorrSigningKey::from_bytes(&extended_bytes).unwrap();

        assert_eq!(sk, sk_restored);
    }

    #[test]
    fn to_bytes_is_deterministic() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);

        let bytes1 = sk.to_bytes();
        let bytes2 = sk.to_bytes();

        assert_eq!(bytes1, bytes2, "to_bytes should be deterministic");
    }

    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 32] = &[
            126, 191, 239, 197, 88, 151, 248, 254, 187, 143, 86, 35, 29, 62, 90, 13, 196, 71, 234,
            5, 90, 124, 205, 194, 51, 192, 228, 133, 25, 140, 157, 7,
        ];

        fn golden_value() -> SchnorrSigningKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            SchnorrSigningKey::generate(&mut rng)
        }

        #[test]
        fn golden_conversions() {
            let value = SchnorrSigningKey::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value().0, value.0);

            let serialized = SchnorrSigningKey::to_bytes(&value);
            let golden_serialized = SchnorrSigningKey::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;

        const GOLDEN_JSON: &str = r#"[126, 191, 239, 197, 88, 151, 248, 254, 187, 143, 86, 35, 29, 62, 90, 13, 196, 71, 234, 5, 90, 124, 205, 194, 51, 192, 228, 133, 25, 140, 157, 7]"#;

        fn golden_value() -> SchnorrSigningKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            SchnorrSigningKey::generate(&mut rng)
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
