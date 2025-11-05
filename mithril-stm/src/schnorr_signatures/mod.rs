pub use midnight_curves::{
    Bls12, EDWARDS_D, Fq as JubjubBase, Fq as BlsScalar, Fr as JubjubScalar,
    G1Affine as BlstG1Affine, G1Projective as BlstG1, G2Affine as BlstG2Affine, JubjubAffine,
    JubjubExtended as Jubjub, JubjubExtended, JubjubSubgroup, MODULUS,
};

use midnight_circuits::{
    ecc::{hash_to_curve::HashToCurveGadget, native::EccChip},
    hash::poseidon::PoseidonChip,
    instructions::{HashToCurveCPU, hash::HashCPU},
    types::AssignedNative,
};

use ff::Field;
use group::Group;
use sha2::{Digest, Sha256};

use subtle::{Choice, ConstantTimeEq};
use thiserror::Error;

pub mod helper;
mod signature;
mod signing_key;
mod verification_key;

pub use helper::*;
pub use signature::*;
pub use signing_key::*;
pub use verification_key::*;

type JubjubHashToCurve = HashToCurveGadget<
    JubjubBase,
    Jubjub,
    AssignedNative<JubjubBase>,
    PoseidonChip<JubjubBase>,
    EccChip<Jubjub>,
>;

type PoseidonHash = PoseidonChip<JubjubBase>;

pub(crate) const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([2u64, 0, 0, 0]);

#[derive(Debug, Error)]
pub enum SignatureError {
    #[error("Verification failed: Signature is invalid.")]
    VerificationFailed,
    /// This error occurs when the serialization of the raw bytes failed
    #[error("Invalid bytes")]
    SerializationError,
}

fn u64s_from_bytes(bytes: &[u8; 32]) -> [u64; 4] {
    [
        u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
        u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
        u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
        u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
    ]
}

// TODO: change msg and seed to random values
#[cfg(test)]
mod tests {
    // use blst::{blst_p1, blst_p2};
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{OsRng, RngCore, SeedableRng};

    // use crate::bls_multi_signature::helper::unsafe_helpers::{p1_affine_to_sig, p2_affine_to_vk};
    use crate::error::{MultiSignatureError, RegisterError};
    use crate::key_registration::KeyRegistration;

    use blake2::{
        Blake2b, Blake2b512, Blake2s256,
        digest::{Digest, FixedOutput, consts::U32},
    };

    type Blake2b256 = Blake2b<U32>;

    use super::*;

    impl PartialEq for SchnorrSigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.to_bytes() == other.to_bytes()
        }
    }

    impl PartialEq for SchnorrVerificationKey {
        fn eq(&self, other: &Self) -> bool {
            self.to_bytes() == other.to_bytes()
        }
    }

    impl Eq for SchnorrSigningKey {}

    // Testing conversion from arbitrary message to base field element
    #[test]
    fn test_hash_msg_to_bas() {
        let msg = vec![0, 0, 0, 1];
        let h = hash_msg_to_base(&msg);
        println!("{:?}", h);
    }

    // Testing basic signature using Sha256 to hash the message
    #[test]
    fn test_sig() {
        let msg = vec![0, 0, 0, 1];
        let mut rng = OsRng;

        let sk = SchnorrSigningKey::generate(&mut ChaCha20Rng::from_entropy());
        let vk = SchnorrVerificationKey::from(&sk);
        let sig = sk.sign(&msg, &mut rng);
        println!("{:?}", sig.clone().to_bytes());
        let sig_bytes = [2, 25, 144, 67, 3, 221, 175, 238, 228, 69, 48, 49, 107, 27, 40, 89, 114, 228, 242, 
            40, 19, 26, 194, 227, 39, 148, 16, 74, 174, 210, 106, 24, 147, 119, 52, 3, 242, 155, 134, 197, 98, 2, 226,
            186, 137, 3, 35, 122, 133, 85, 244, 147, 3, 9, 152, 12, 119, 43, 16, 119, 26, 72, 158, 5, 25, 7, 168, 188, 
            29, 43, 115, 214, 175, 85, 221, 181, 101, 39, 224, 15, 243, 141, 37, 100, 49, 179, 92, 96, 34, 201, 120, 160, 
            56, 91, 35, 111];
        let correct_sig = SchnorrSignature::from_bytes(&sig_bytes);
        sig.verify(&msg, &vk).unwrap();
    }

    // Testing basic signature using Blake2b256 to hash the message
    #[test]
    fn test_sig_blake() {
        let mut rng = OsRng;
        let msg = vec![0, 0, 0, 1];
        let sk = SchnorrSigningKey::generate(&mut ChaCha20Rng::from_entropy());
        let vk = SchnorrVerificationKey::from(&sk);

        let sig = sk.sign(&msg, &mut rng);
        sig.verify(&msg, &vk).unwrap();
    }

    /// Test signing functionality.
    #[test]
    fn test_signature_verification_valid() {
        let msg = vec![0, 0, 0, 1];
        let mut rng = OsRng;
        let sk = SchnorrSigningKey::generate(&mut rng);
        // let msg = JubjubBase::random(&mut rng);

        // Sign the message
        let signature = sk.sign(&msg, &mut rng);

        // Ensure the components of the signature are non-default values
        assert_ne!(
            signature.sigma,
            JubjubSubgroup::identity(),
            "Signature sigma should not be the identity element."
        );
        assert_ne!(
            signature.s,
            JubjubScalar::ZERO,
            "Signature s component should not be zero."
        );
        assert_ne!(
            signature.c,
            JubjubBase::ZERO,
            "Signature c component should not be zero."
        );

        signature.verify(&msg, &SchnorrVerificationKey::from(&sk)).unwrap();
    }

    #[test]
    fn test_signature_verification_invalid_signature() {
        let mut rng = OsRng;
        let sk = SchnorrSigningKey::generate(&mut rng);
        let msg = vec![0, 0, 0, 1];
        let vk: SchnorrVerificationKey = (&sk).into();

        // Generate signature and tamper with it
        let mut signature = sk.sign(&msg, &mut rng);
        signature.s = JubjubScalar::random(&mut rng); // Modify `s` component

        // Verify the modified signature
        let result = signature.verify(&msg, &vk);
        assert!(
            result.is_err(),
            "Invalid signature should fail verification, but it passed."
        );
    }

    #[test]
    fn serialize_deserialize_vk() {
        let seed = 0;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::from(&sk);
        let vk_bytes = vk.to_bytes();
        let vk2 = SchnorrVerificationKey::from_bytes(&vk_bytes).unwrap();
        assert_eq!(vk, vk2);
    }

    #[test]
    fn serialize_deserialize_sk() {
        let seed = 0;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let sk_bytes: [u8; 32] = sk.to_bytes();
        let sk2 = SchnorrSigningKey::from_bytes(&sk_bytes).unwrap();
        assert_eq!(sk, sk2);
    }

    #[test]
    fn serialize_deserialize_signature() {
        let seed = 0;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        let msg = vec![0, 0, 0, 1];
        let sk = SchnorrSigningKey::generate(&mut rng);

        let sig = sk.sign(&msg, &mut rng);

        let sig_bytes: [u8; 96] = sig.clone().to_bytes();
        let sig2 = SchnorrSignature::from_bytes(&sig_bytes).unwrap();
        assert_eq!(sig, sig2);
    }
}
