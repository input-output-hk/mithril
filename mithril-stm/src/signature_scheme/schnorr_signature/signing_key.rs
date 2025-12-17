use anyhow::{Context, anyhow};
use rand_core::{CryptoRng, RngCore};
use serde::{Deserialize, Serialize};

use super::{
    BaseFieldElement, PrimeOrderProjectivePoint, ProjectivePoint, ScalarFieldElement,
    SchnorrSignature, SchnorrSignatureError, SchnorrVerificationKey, compute_truncated_digest,
};
use crate::StmResult;

/// Schnorr Signing key, it is essentially a random scalar of the Jubjub scalar field
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct SchnorrSigningKey(pub(crate) ScalarFieldElement);

impl SchnorrSigningKey {
    /// Generate a random scalar value to use as signing key
    pub fn generate<R: RngCore + CryptoRng>(rng: &mut R) -> StmResult<Self> {
        let scalar = ScalarFieldElement::new_random_nonzero_scalar(rng)
            .with_context(|| "Failed to generate a non zero Schnorr signing key.")?;
        Ok(SchnorrSigningKey(scalar))
    }

    /// This function is an adapted version of the Schnorr signature scheme that includes
    /// the computation of a deterministic value (called commitment_point) based on the message and the signing key
    /// and works with the Jubjub elliptic curve and the Poseidon hash function.
    ///
    /// Input:
    ///     - a message: some bytes
    ///     - a secret key: an element of the scalar field of the Jubjub curve
    /// Output:
    ///     - a signature of the form (commitment_point, response, challenge):
    ///         - commitment_point is deterministic depending only on the message and secret key
    ///         - the response and challenge depends on a random value generated during the signature
    ///
    /// The protocol computes:
    ///     - commitment_point = secret_key * H(Sha256(msg))
    ///     - random_scalar, a random value
    ///     - random_point_1 = random_scalar * H(Sha256(msg))
    ///     - random_point_2 = random_scalar * prime_order_generator_point, where generator is a generator of the prime-order subgroup of Jubjub
    ///     - challenge = Poseidon(DST || H(Sha256(msg)) || verification_key || commitment_point || random_point_1 || random_point_2)
    ///     - response = random_scalar - challenge * signing_key
    ///
    /// Output the signature (commitment_point, response, challenge)
    ///
    pub fn sign<R: RngCore + CryptoRng>(
        &self,
        msg: &[u8],
        rng: &mut R,
    ) -> StmResult<SchnorrSignature> {
        // Use the subgroup generator to compute the curve points
        let prime_order_generator_point = PrimeOrderProjectivePoint::create_generator();
        let verification_key = SchnorrVerificationKey::new_from_signing_key(self.clone())
            .with_context(|| "Could not generate verification key from signing key.")?;

        // First hashing the message to a scalar then hashing it to a curve point
        let msg_hash_point = ProjectivePoint::hash_to_projective_point(msg);

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

        let challenge = compute_truncated_digest(&points_coordinates);
        let challenge_times_sk = challenge * self.0;
        let response = random_scalar - challenge_times_sk;

        Ok(SchnorrSignature {
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
            return Err(anyhow!(SchnorrSignatureError::SerializationError)).with_context(
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

    mod golden {

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::signature_scheme::SchnorrSigningKey;

        const GOLDEN_JSON: &str = r#"[126, 191, 239, 197, 88, 151, 248, 254, 187, 143, 86, 35, 29, 62, 90, 13, 196, 71, 234, 5, 90, 124, 205, 194, 51, 192, 228, 133, 25, 140, 157, 7]"#;

        fn golden_value() -> SchnorrSigningKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            SchnorrSigningKey::generate(&mut rng).unwrap()
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
