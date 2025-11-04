pub use midnight_curves::{
    Bls12, EDWARDS_D, Fq as JubjubBase, Fq as BlsScalar, Fr as JubjubScalar,
    G1Affine as BlstG1Affine, G1Projective as BlstG1, G2Affine as BlstG2Affine, JubjubAffine,
    JubjubExtended as Jubjub, JubjubExtended, JubjubSubgroup, MODULUS,
};

use midnight_circuits::{
    ecc::{
        hash_to_curve::HashToCurveGadget,
        native::EccChip,
    },
    hash::poseidon::PoseidonChip,
    instructions::{
        HashToCurveCPU,
        hash::HashCPU,
    },
    types::AssignedNative,
};

use ff::{Field};
use group::Group;

use subtle::{Choice, ConstantTimeEq};
use thiserror::Error;

pub mod helper;
mod signature;
mod signing_key;
mod verification_key;

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



#[cfg(test)]
mod tests {
    // use blst::{blst_p1, blst_p2};
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng, OsRng};

    // use crate::bls_multi_signature::helper::unsafe_helpers::{p1_affine_to_sig, p2_affine_to_vk};
    use crate::error::{MultiSignatureError, RegisterError};
    use crate::key_registration::KeyRegistration;

    use super::*;

    impl PartialEq for SchnorrSigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.to_bytes() == other.to_bytes()
        }
    }

    // impl Eq for SchnorrSigningKey {}

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(1000))]
    
        /// Test signing functionality.
        #[test]
        fn test_signature_verification_valid(seed in any::<u64>()) {
            let mut rng = OsRng;
            let sk = SchnorrSigningKey::generate(&mut rng);
            let msg = JubjubBase::random(&mut rng);

            // Sign the message
            let signature = sk.sign(msg, &mut rng);

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

            signature.verify(msg, &SchnorrVerificationKey::from(&sk)).unwrap();
        }

        #[test]
        fn test_signature_verification_invalid_signature(seed in any::<u64>()) {
            let mut rng = OsRng;
            let sk = SchnorrSigningKey::generate(&mut rng);
            let msg = JubjubBase::random(&mut rng);
            let vk: SchnorrVerificationKey = (&sk).into();

            // Generate signature and tamper with it
            let mut signature = sk.sign(msg, &mut rng);
            signature.s = JubjubScalar::random(&mut rng); // Modify `s` component

            // Verify the modified signature
            let result = signature.verify(msg, &vk);
            assert!(
                result.is_err(),
                "Invalid signature should fail verification, but it passed."
            );
        }
    
    
    }


}