use anyhow::{Context, anyhow};
use serde::{Deserialize, Serialize};

use super::{
    BaseFieldElement, PrimeOrderProjectivePoint, ProjectivePoint, ScalarFieldElement,
    SchnorrVerificationKey, UniqueSchnorrSignatureError, compute_poseidon_digest,
};
use crate::StmResult;

/// Structure of the Unique Schnorr signature to use with the SNARK
///
/// This signature includes a value `commitment_point` which depends only on
/// the message and the signing key.
/// This value is used in the lottery process to determine the correct indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct UniqueSchnorrSignature {
    /// Deterministic value depending on the message and signing key
    pub(crate) commitment_point: ProjectivePoint,
    /// Part of the Unique Schnorr signature depending on the signing key
    pub(crate) response: ScalarFieldElement,
    /// Part of the Unique Schnorr signature NOT depending on the signing key
    /// TODO: Change to BaseFieldElement
    pub(crate) challenge: ScalarFieldElement,
}

impl UniqueSchnorrSignature {
    /// This function performs the verification of a Unique Schnorr signature given the signature, the signed message
    /// and a verification key derived from the signing key used to sign.
    ///
    /// Input:
    ///     - a Unique Schnorr signature
    ///     - a message: some bytes
    ///     - a verification key: a value depending on the signing key
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
        let msg_hash_point = ProjectivePoint::hash_to_projective_point(msg)?;

        // Computing random_point_1_recomputed = response *  H(msg) + challenge * commitment_point
        // let challenge_scalar = ScalarFieldElement::from_base_field(&self.challenge);
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

        let challenge_recomputed = compute_poseidon_digest(&points_coordinates);
        let challenge_recomputed_scalar =
            ScalarFieldElement::from_base_field(&challenge_recomputed)?;

        if challenge_recomputed != self.challenge {
            return Err(anyhow!(UniqueSchnorrSignatureError::SignatureInvalid(
                Box::new(*self)
            )));
        }

        Ok(())
    }

    /// Convert a `UniqueSchnorrSignature` into bytes.
    pub fn to_bytes(self) -> [u8; 96] {
        let mut out = [0; 96];
        out[0..32].copy_from_slice(&self.commitment_point.to_bytes());
        out[32..64].copy_from_slice(&self.response.to_bytes());
        out[64..96].copy_from_slice(&self.challenge.to_bytes());

        out
    }

    /// Convert bytes into a `UniqueSchnorrSignature`.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 96 {
            return Err(anyhow!(UniqueSchnorrSignatureError::Serialization))
                .with_context(|| "Not enough bytes provided to create a signature.");
        }

        let commitment_point = ProjectivePoint::from_bytes(
            bytes
                .get(0..32)
                .ok_or(UniqueSchnorrSignatureError::Serialization)
                .with_context(|| "Could not get the bytes of `commitment_point`")?,
        )
        .with_context(|| "Could not convert bytes to `commitment_point`")?;

        let response = ScalarFieldElement::from_bytes(
            bytes
                .get(32..64)
                .ok_or(UniqueSchnorrSignatureError::Serialization)
                .with_context(|| "Could not get the bytes of `response`")?,
        )
        .with_context(|| "Could not convert the bytes to `response`")?;

        let challenge = ScalarFieldElement::from_bytes(
            bytes
                .get(64..96)
                .ok_or(UniqueSchnorrSignatureError::Serialization)
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
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{
        SchnorrSigningKey, SchnorrVerificationKey, UniqueSchnorrSignature,
    };

    #[test]
    fn valid_signature_verification() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone()).unwrap();

        let sig = sk.sign(&msg, &mut rng).unwrap();

        sig.verify(&msg, &vk)
            .expect("Valid signature should verify successfully");
    }

    #[test]
    fn invalid_signature() {
        let msg = vec![0, 0, 0, 1];
        let msg2 = vec![0, 0, 0, 2];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone()).unwrap();
        let sk2 = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk2 = SchnorrVerificationKey::new_from_signing_key(sk2).unwrap();

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig2 = sk.sign(&msg2, &mut rng).unwrap();

        // Wrong verification key is used
        let result1 = sig.verify(&msg, &vk2);
        let result2 = sig2.verify(&msg, &vk);

        result1.expect_err("Wrong verification key used, test should fail.");
        // Wrong message is verified
        result2.expect_err("Wrong message used, test should fail.");
    }

    #[test]
    fn from_bytes_signature_not_enough_bytes() {
        let msg = vec![0u8; 95];
        let result = UniqueSchnorrSignature::from_bytes(&msg);
        result.expect_err("Not enough bytes.");
    }

    #[test]
    fn from_bytes_signature_exact_size() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let msg = vec![1, 2, 3];
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig_bytes: [u8; 96] = sig.to_bytes();

        let sig_restored = UniqueSchnorrSignature::from_bytes(&sig_bytes).unwrap();
        assert_eq!(sig, sig_restored);
    }

    #[test]
    fn from_bytes_signature_extra_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let msg = vec![1, 2, 3];
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig_bytes: [u8; 96] = sig.to_bytes();

        let mut extended_bytes = sig_bytes.to_vec();
        extended_bytes.extend_from_slice(&[0xFF; 10]);

        let sig_restored = UniqueSchnorrSignature::from_bytes(&extended_bytes).unwrap();
        assert_eq!(sig, sig_restored);
    }

    #[test]
    fn to_bytes_is_deterministic() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let msg = vec![1, 2, 3];
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();

        let sig = sk.sign(&msg, &mut rng).unwrap();

        // Converting to bytes multiple times should give same result
        let bytes1 = sig.to_bytes();
        let bytes2 = sig.to_bytes();

        assert_eq!(bytes1, bytes2);
    }

    #[test]
    fn signature_roundtrip_preserves_verification() {
        let mut rng = ChaCha20Rng::from_seed([42u8; 32]);
        let msg = vec![5, 6, 7, 8, 9];
        let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone()).unwrap();

        // Create and verify original signature
        let sig = sk.sign(&msg, &mut rng).unwrap();
        sig.verify(&msg, &vk).expect("Original signature should verify");

        // Roundtrip through bytes
        let sig_bytes = sig.to_bytes();
        let sig_restored = UniqueSchnorrSignature::from_bytes(&sig_bytes).unwrap();

        // Restored signature should still verify
        sig_restored
            .verify(&msg, &vk)
            .expect("Restored signature should verify");
    }

    mod golden {
        use super::*;

        const GOLDEN_BYTES: &[u8; 96] = &[
            143, 53, 198, 62, 178, 1, 88, 253, 21, 92, 100, 13, 72, 180, 198, 127, 39, 175, 102,
            69, 147, 249, 244, 224, 122, 121, 248, 68, 217, 242, 158, 113, 5, 81, 137, 228, 235,
            18, 112, 76, 71, 127, 44, 47, 60, 55, 144, 204, 254, 50, 67, 167, 67, 133, 79, 168, 10,
            153, 228, 114, 147, 64, 34, 9, 12, 75, 91, 200, 29, 62, 12, 245, 185, 181, 67, 251,
            210, 211, 37, 42, 204, 205, 133, 215, 235, 236, 193, 155, 2, 147, 83, 189, 148, 38, 71,
            0,
        ];

        fn golden_value() -> UniqueSchnorrSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            let msg = [0u8; 32];
            sk.sign(&msg, &mut rng).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = UniqueSchnorrSignature::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = UniqueSchnorrSignature::to_bytes(value);
            let golden_serialized = UniqueSchnorrSignature::to_bytes(golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }

    mod golden_json {
        use super::*;

        const GOLDEN_JSON: &str = r#"
        {
            "commitment_point": [143, 53, 198, 62, 178, 1, 88, 253, 21, 92, 100, 13, 72, 180, 198, 127, 39, 175, 102, 69, 147, 249, 244, 224, 122, 121, 248, 68, 217, 242, 158, 113],
            "response": [5, 81, 137, 228, 235, 18, 112, 76, 71, 127, 44, 47, 60, 55, 144, 204, 254, 50, 67, 167, 67, 133, 79, 168, 10, 153, 228, 114, 147, 64, 34, 9],
            "challenge": [12, 75, 91, 200, 29, 62, 12, 245, 185, 181, 67, 251, 210, 211, 37, 42, 204, 205, 133, 215, 235, 236, 193, 155, 2, 147, 83, 189, 148, 38, 71, 0]
        }"#;

        fn golden_value() -> UniqueSchnorrSignature {
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
