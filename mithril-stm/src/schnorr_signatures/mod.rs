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

pub const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([2u64, 0, 0, 0]);


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
    use super::*;
    use rand_core::OsRng;

    /// Test signing functionality.
    #[test]
    fn test_signature_verification_valid() {
        let mut rng = OsRng;
        let sk = SigningKey::generate(&mut rng);
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

        signature.verify(msg, &VerificationKey::from(&sk)).unwrap();
    }

    #[test]
    fn test_signature_verification_invalid_signature() {
        let mut rng = OsRng;
        let sk = SigningKey::generate(&mut rng);
        let msg = JubjubBase::random(&mut rng);
        let vk: VerificationKey = (&sk).into();

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