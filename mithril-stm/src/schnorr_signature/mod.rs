// TODO: Remove
#![allow(dead_code)]

mod signature;
mod signing_key;
mod utils;
mod verification_key;

use midnight_circuits::{
    ecc::{hash_to_curve::HashToCurveGadget, native::EccChip},
    hash::poseidon::PoseidonChip,
    types::AssignedNative,
};
use midnight_curves::{Fq as JubjubBase, JubjubExtended};

use signature::*;
use utils::*;
use verification_key::*;

/// A DST (Domain Separation Tag) to distinguish between use of Poseidon hash
pub const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([0u64, 0, 0, 0]);
pub const DST_LOTTERY: JubjubBase = JubjubBase::from_raw([1u64, 0, 0, 0]);

/// Defining a type for the CPU hash to curve gadget
pub(crate) type JubjubHashToCurve = HashToCurveGadget<
    JubjubBase,
    JubjubExtended,
    AssignedNative<JubjubBase>,
    PoseidonChip<JubjubBase>,
    EccChip<JubjubExtended>,
>;

#[cfg(test)]
mod tests {

    use super::*;
    use group::Group;
    use midnight_curves::Fr as JubjubScalar;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::schnorr_signature::{
        signing_key::SchnorrSigningKey, verification_key::SchnorrVerificationKey,
    };

    // Testing conversion from arbitrary message to scalar field element
    #[test]
    fn test_hash_msg_to_jubjubbase() {
        let msg = vec![0, 0, 0, 1];
        let h = hash_msg_to_jubjubbase(&msg).unwrap();
        // Correct value corresponding to the message [0,0,0,1]
        let bytes_le = [
            179, 7, 17, 168, 141, 112, 57, 117, 112, 92, 169, 56, 36, 70, 1, 217, 9, 13, 255, 42,
            100, 207, 166, 110, 188, 47, 35, 211, 35, 168, 100, 25,
        ];
        let field_elem = JubjubBase::from_bytes_le(&bytes_le).unwrap();
        assert_eq!(h, field_elem)
    }

    // Testing conversion from EC point to scalar coordinates
    // For now only printing, next step is to try to generate a point
    // from x and y values to check if they match with the result of the function
    #[test]
    fn test_get_coordinates() {
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let point = JubjubSubgroup::random(&mut rng);
        let (_x, _y) = get_coordinates(point);
        // println!("{:?}", (x, y));
    }

    // Testing conversion from BLS12-381 base field to Jubjub base field
    // TODO: Add randomness to val
    #[test]
    fn test_jubjub_base_to_scalar() {
        let val = vec![0, 0, 0, 1];
        let jjbase = JubjubBase::from_raw(val.clone().try_into().unwrap());
        let jjscalar = JubjubScalar::from_raw(val.try_into().unwrap());

        let converted_base = jubjub_base_to_scalar(&jjbase).unwrap();

        assert_eq!(jjscalar, converted_base);
    }

    #[test]
    fn test_sig_and_verify() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::from(&sk);
        let sig = sk.sign(&msg, &mut rng).unwrap();
        sig.verify(&msg, &vk).unwrap();
    }

    #[test]
    fn test_invalid_sig() {
        let msg = vec![0, 0, 0, 1];
        let msg2 = vec![0, 0, 0, 2];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);

        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::from(&sk);

        let sk2 = SchnorrSigningKey::generate(&mut rng);
        let vk2 = SchnorrVerificationKey::from(&sk2);

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig2 = sk.sign(&msg2, &mut rng).unwrap();

        // Wrong verification key is used
        let result = sig.verify(&msg, &vk2);
        assert!(
            result.is_err(),
            "Wrong verfication key used, test should fail."
        );

        // Wrong message is verified
        let result = sig2.verify(&msg, &vk);
        assert!(result.is_err(), "Wrong message used, test should fail.");
    }

    #[test]
    fn serialize_deserialize_vk() {
        let seed = 0;
        let mut rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let vk = SchnorrVerificationKey::from(&sk);
        let vk_bytes = vk.to_bytes();
        let vk2 = SchnorrVerificationKey::from_bytes(&vk_bytes).unwrap();
        assert_eq!(vk.0, vk2.0);
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

        let sig = sk.sign(&msg, &mut rng).unwrap();

        let sig_bytes: [u8; 96] = sig.clone().to_bytes();
        let sig2 = SchnorrSignature::from_bytes(&sig_bytes).unwrap();
        assert_eq!(sig, sig2);
    }
}
