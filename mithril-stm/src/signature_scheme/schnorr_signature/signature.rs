use anyhow::{Context, anyhow};
use serde::{Deserialize, Serialize};

use super::{
    BaseFieldElement, PrimeOrderProjectivePoint, ProjectivePoint, ScalarFieldElement,
    SchnorrSignatureError, SchnorrVerificationKey, compute_truncated_digest,
};
use crate::StmResult;

/// Structure of the Schnorr signature to use with the SNARK
///
/// This signature includes a value `commitment_point` which depends only on
/// the message and the signing key.
/// This value is used in the lottery process to determine the correct indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SchnorrSignature {
    /// Deterministic value depending on the message and secret key
    pub(crate) commitment_point: ProjectivePoint,
    /// Part of the Schnorr signature depending on the secret key
    pub(crate) response: ScalarFieldElement,
    /// Part of the Schnorr signature NOT depending on the secret key
    pub(crate) challenge: ScalarFieldElement,
}

impl SchnorrSignature {
    /// This function performs the verification of a Schnorr signature given the signature, the signed message
    /// and a verification key derived from the secret key used to sign.
    ///
    /// Input:
    ///     - a Schnorr signature
    ///     - a message: some bytes
    ///     - a verification key: a value depending on the secret key
    /// Output:
    ///     - Ok(()) if the signature verifies and an error if not
    ///
    /// The protocol computes:
    ///     - msg_hash_point = H(Sha256(msg))
    ///     - random_point_1_recomputed = response * msg_hash_point + challenge * commitment_point
    ///     - random_point_2_recomputed = response * prime_order_generator_point + challenge * verification_key
    ///     - challenge_recomputed = Poseidon(DST || H(Sha256(msg)) || verification_key
    ///     || commitment_point || random_point_1_recomputed || random_point_2_recomputed)
    ///
    /// Check: challenge == challenge_recomputed
    ///
    pub fn verify(&self, msg: &[u8], verification_key: &SchnorrVerificationKey) -> StmResult<()> {
        // Check that the verification key is valid
        verification_key
            .is_valid()
            .with_context(|| "Signature verification failed due to invalid verification key")?;

        let prime_order_generator_point = PrimeOrderProjectivePoint::create_generator();

        // First hashing the message to a scalar then hashing it to a curve point
        let msg_hash_point = ProjectivePoint::hash_to_projective_point(msg);

        // Computing random_point_1_recomputed = response *  H(msg) + challenge * commitment_point
        let random_point_1_recomputed =
            self.response * msg_hash_point + self.challenge * self.commitment_point;

        // Computing random_point_2_recomputed = response * prime_order_generator_point + challenge * vk
        let random_point_2_recomputed =
            self.response * prime_order_generator_point + self.challenge * verification_key.0;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        let points_coordinates: Vec<BaseFieldElement> = [
            msg_hash_point,
            ProjectivePoint::from(verification_key.0),
            self.commitment_point,
            random_point_1_recomputed,
            ProjectivePoint::from(random_point_2_recomputed),
        ]
        .iter()
        .flat_map(|point| {
            let (u, v) = point.get_coordinates();
            [u, v]
        })
        .collect();

        let challenge_recomputed = compute_truncated_digest(&points_coordinates);

        if challenge_recomputed != self.challenge {
            return Err(anyhow!(SchnorrSignatureError::SignatureInvalid(Box::new(
                *self
            ))));
        }

        Ok(())
    }

    /// Convert a `SchnorrSignature` into bytes.
    pub fn to_bytes(self) -> [u8; 96] {
        let mut out = [0; 96];
        out[0..32].copy_from_slice(&self.commitment_point.to_bytes());
        out[32..64].copy_from_slice(&self.response.to_bytes());
        out[64..96].copy_from_slice(&self.challenge.to_bytes());

        out
    }

    /// Convert bytes into a `SchnorrSignature`.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 96 {
            return Err(anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Not enough bytes provided to create a signature.");
        }

        let commitment_point = ProjectivePoint::from_bytes(
            bytes
                .get(0..32)
                .ok_or(SchnorrSignatureError::SerializationError)
                .with_context(|| "Could not get the bytes of `commitment_point`")?,
        )
        .with_context(|| "Could not convert bytes to `commitment_point`")?;

        let response = ScalarFieldElement::from_bytes(
            bytes
                .get(32..64)
                .ok_or(SchnorrSignatureError::SerializationError)
                .with_context(|| "Could not get the bytes of `response`")?,
        )
        .with_context(|| "Could not convert the bytes to `response`")?;

        let challenge = ScalarFieldElement::from_bytes(
            bytes
                .get(64..96)
                .ok_or(SchnorrSignatureError::SerializationError)
                .with_context(|| "Could not get the bytes of `challenge`")?,
        )
        .with_context(|| "Could not convert bytes to `challenge`")?;

        Ok(Self {
            commitment_point,
            response,
            challenge,
        })
    }
}

#[cfg(test)]
mod tests {

    mod golden {

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::signature_scheme::{SchnorrSignature, SchnorrSigningKey};

        const GOLDEN_JSON: &str = r#"
        {
            "commitment_point": [143, 53, 198, 62, 178, 1, 88, 253, 21, 92, 100, 13, 72, 180, 198, 127, 39, 175, 102, 69, 147, 249, 244, 224, 122, 121, 248, 68, 217, 242, 158, 113],
            "response": [94, 57, 200, 241, 208, 145, 251, 8, 92, 119, 163, 38, 81, 85, 54, 36, 193, 221, 254, 242, 21, 129, 110, 161, 142, 184, 107, 156, 100, 34, 190, 9],
            "challenge": [200, 20, 178, 142, 61, 253, 193, 11, 5, 180, 97, 73, 125, 88, 162, 36, 30, 177, 225, 52, 136, 21, 138, 93, 81, 23, 19, 64, 82, 78, 229, 3]
        }"#;

        fn golden_value() -> SchnorrSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            let msg = [0u8; 32];
            sk.sign(&msg, &mut rng).unwrap()
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
