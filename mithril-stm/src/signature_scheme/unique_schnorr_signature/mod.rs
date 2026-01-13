//! Unique Schnorr Signature module
//!
//! This module implements a variant of the classical Schnorr signature algorithm.
//! The classical Schnorr signature uses random values to generate the signature which is
//! then also random and thus not unique for a given pair (message, signing key).
//! In this variant, we compute an additional unique value compared to the regular scheme
//! that is based only on the pair (secret key, message). This new part of the signature
//! is not based on randomness and is thus deterministic and adds a unique identification
//! tag to each signature.

mod error;
mod jubjub;
mod signature;
mod signing_key;
mod verification_key;

pub use error::*;
pub(crate) use jubjub::*;
pub use signature::*;
pub use signing_key::*;
pub use verification_key::*;

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{
        PrimeOrderProjectivePoint, ScalarFieldElement, SchnorrSigningKey, SchnorrVerificationKey,
        UniqueSchnorrSignature, UniqueSchnorrSignatureError,
    };

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn verification_key(seed in any::<[u8;32]>()) {
            // Valid generation check
            let sk = SchnorrSigningKey::generate(&mut ChaCha20Rng::from_seed(seed)).unwrap();
            let g = PrimeOrderProjectivePoint::create_generator();
            let vk = sk.0 * g;
            let vk_from_sk = SchnorrVerificationKey::new_from_signing_key(sk).unwrap();
            assert_eq!(vk, vk_from_sk.0);

            // Check if sk is 0
            let mut bytes = [0u8; 32];
            let sk = SchnorrSigningKey(ScalarFieldElement::from_bytes(&bytes).unwrap());
            SchnorrVerificationKey::new_from_signing_key(sk).expect_err("Verification key should not be generated from zero signing key");

            // Check if sk is 1
            bytes[0] = 1;
            let sk = SchnorrSigningKey(ScalarFieldElement::from_bytes(&bytes).unwrap());
            SchnorrVerificationKey::new_from_signing_key(sk).expect_err("Verification key should not be generated from one signing key");
        }

        #[test]
        fn valid_signing_verification(
            msg in prop::collection::vec(any::<u8>(), 1..128),
            seed in any::<[u8;32]>(),
        ) {
            let sk_result = SchnorrSigningKey::generate(&mut ChaCha20Rng::from_seed(seed));
            assert!(sk_result.is_ok(), "Secret ket generation failed");
            let sk = sk_result.unwrap();

            let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone()).unwrap();

            let sig_result = sk.sign(&msg, &mut ChaCha20Rng::from_seed(seed));
            assert!(sig_result.is_ok(), "Signature generation failed");

            let sig = sig_result.unwrap();

            assert!(sig.verify(&msg, &vk).is_ok(), "Verification failed.");
        }

        #[test]
        fn invalid_signature(msg in prop::collection::vec(any::<u8>(), 1..128), seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk1 = SchnorrSigningKey::generate(&mut rng).unwrap();
            let vk1 = SchnorrVerificationKey::new_from_signing_key(sk1).unwrap();
            let sk2 = SchnorrSigningKey::generate(&mut rng).unwrap();
            let fake_sig = sk2.sign(&msg, &mut rng).unwrap();

            let error = fake_sig.verify(&msg, &vk1).expect_err("Fake signature should not be verified");

            assert!(
                matches!(
                    error.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::SignatureInvalid(_))
                ),
                "Unexpected error: {error:?}"
            );
        }

        #[test]
        fn signing_key_to_from_bytes(seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            let mut sk_bytes = sk.to_bytes();

            // Valid conversion
            let recovered_sk = SchnorrSigningKey::from_bytes(&sk_bytes).unwrap();
            assert_eq!(sk.0, recovered_sk.0, "Recovered signing key does not match with the original!");

            // Not enough bytes
            let mut short_bytes = [0u8; 31];
            short_bytes.copy_from_slice(sk_bytes.get(..31).unwrap());
            let result = SchnorrSigningKey::from_bytes(&short_bytes).expect_err("From bytes conversion of signing key should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::Serialization)
                ),
                "Unexpected error: {result:?}"
            );

            // Invalid bytes
            sk_bytes[31] |= 0xff;
            let result = SchnorrSigningKey::from_bytes(&sk_bytes).expect_err("From bytes conversion of signing key should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::ScalarFieldElementSerialization)
                ),
                "Unexpected error: {result:?}"
            );
        }

        #[test]
        fn verification_key_to_from_bytes(seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            let vk = SchnorrVerificationKey::new_from_signing_key(sk.clone()).unwrap();
            let mut vk_bytes = vk.to_bytes();

            // Valid conversion
            let recovered_vk = SchnorrVerificationKey::from_bytes(&vk_bytes).unwrap();
            assert_eq!(vk.0, recovered_vk.0, "Recovered verification key does not match with the original!");

            // Not enough bytes
            let mut short_bytes = [0u8; 31];
            short_bytes.copy_from_slice(vk_bytes.get(..31).unwrap());
            let result = SchnorrVerificationKey::from_bytes(&short_bytes).expect_err("From bytes conversion of verification key should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::Serialization)
                ),
                "Unexpected error: {result:?}"
            );

            // Invalid bytes
            vk_bytes[31] |= 0xff;
            let result = SchnorrVerificationKey::from_bytes(&vk_bytes).expect_err("From bytes conversion of verification key should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::PrimeOrderProjectivePointSerialization)
                ),
                "Unexpected error: {result:?}"
            );
        }

        #[test]
        fn signature_to_from_bytes(msg in prop::collection::vec(any::<u8>(), 1..128), seed in any::<[u8;32]>()) {
            let mut rng = ChaCha20Rng::from_seed(seed);
            let sk = SchnorrSigningKey::generate(&mut rng).unwrap();
            let signature = sk.sign(&msg, &mut ChaCha20Rng::from_seed(seed)).unwrap();
            let signature_bytes = signature.to_bytes();

            // Valid conversion
            let recovered_signature = UniqueSchnorrSignature::from_bytes(&signature_bytes).unwrap();
            assert_eq!(signature, recovered_signature, "Recovered signature does not match with the original!");

            // Test invalid `commitment_point`
            let mut corrupted_bytes = signature_bytes;
            corrupted_bytes[31] |= 0xff;
            let result = UniqueSchnorrSignature::from_bytes(&corrupted_bytes).expect_err("From bytes conversion of signature should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::ProjectivePointSerialization)
                ),
                "Unexpected error: {result:?}"
            );

            // Test invalid `response`
            let mut corrupted_bytes = signature_bytes;
            corrupted_bytes[63] |= 0xff;
            let result = UniqueSchnorrSignature::from_bytes(&corrupted_bytes).expect_err("From bytes conversion should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::ScalarFieldElementSerialization)
                ),
                "Unexpected error: {result:?}"
            );

            // Test invalid `challenge`
            let mut corrupted_bytes = signature_bytes;
            corrupted_bytes[95] |= 0xff;
            let result = UniqueSchnorrSignature::from_bytes(&corrupted_bytes).expect_err("From bytes conversion should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::ScalarFieldElementSerialization)
                ),
                "Unexpected error: {result:?}"
            );

            // Not enough bytes
            let mut short_bytes = [0u8; 95];
            short_bytes.copy_from_slice(signature_bytes.get(..95).unwrap());
            let result = UniqueSchnorrSignature::from_bytes(&short_bytes).expect_err("From bytes conversion of signature should fail");
            assert!(
                matches!(
                    result.downcast_ref::<UniqueSchnorrSignatureError>(),
                    Some(UniqueSchnorrSignatureError::Serialization)
                ),
                "Unexpected error: {result:?}"
            );
        }
    }
}
