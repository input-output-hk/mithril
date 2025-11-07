#![allow(dead_code)]

use midnight_circuits::{
    ecc::{hash_to_curve::HashToCurveGadget, native::EccChip},
    hash::poseidon::PoseidonChip,
    types::AssignedNative,
};
use midnight_curves::{
    Fq as JubjubBase, Fr as JubjubScalar, JubjubAffine, JubjubExtended, JubjubSubgroup,
};
use sha2::{Digest, Sha256};

use anyhow::{Result, anyhow};

mod signature;
mod signing_key;
mod verification_key;

/// A DST to distinguish between use of Poseidon hash
pub const DST_SIGNATURE: JubjubBase = JubjubBase::from_raw([2u64, 0, 0, 0]);

/// Defining a type for the CPU hash to curve gadget
type JubjubHashToCurve = HashToCurveGadget<
    JubjubBase,
    JubjubExtended,
    AssignedNative<JubjubBase>,
    PoseidonChip<JubjubBase>,
    EccChip<JubjubExtended>,
>;

/// Convert an arbitrary array of bytes into a Jubjub scalar field element
/// First hash the message to 256 bits use Sha256 then perform the conversion
/// TODO: Handle the unwrap properly
pub(crate) fn hash_msg_to_jubjubbase(msg: &[u8]) -> Result<JubjubBase> {
    let mut hash = Sha256::new();
    hash.update(msg);
    let hmsg = hash.finalize();
    let mut output = [0u8; 32];
    // Adding a check here but this
    if hmsg.len() == output.len() {
        output.copy_from_slice(&hmsg);
    } else {
        return Err(anyhow!(
            "Hash of the message does not have the correct lenght."
        ));
    }
    Ok(JubjubBase::from_raw([
        u64::from_le_bytes(output[0..8].try_into()?),
        u64::from_le_bytes(output[8..16].try_into()?),
        u64::from_le_bytes(output[16..24].try_into()?),
        u64::from_le_bytes(output[24..32].try_into()?),
    ]))
}

/// Extract the coordinates of a given point
/// This is mainly use to feed the Poseidon hash function
pub(crate) fn get_coordinates(point: JubjubSubgroup) -> (JubjubBase, JubjubBase) {
    let extended = JubjubExtended::from(point); // Convert to JubjubExtended
    let affine = JubjubAffine::from(extended); // Convert to JubjubAffine (affine coordinates)
    let x = affine.get_u(); // Get x-coordinate
    let y = affine.get_v(); // Get y-coordinate
    (x, y)
}

/// Convert an element of the BLS12-381 base field to
/// one of the Jubjub base field
pub fn jubjub_base_to_scalar(x: &JubjubBase) -> Result<JubjubScalar> {
    let bytes = x.to_bytes_le();
    Ok(JubjubScalar::from_raw([
        u64::from_le_bytes(bytes[0..8].try_into()?),
        u64::from_le_bytes(bytes[8..16].try_into()?),
        u64::from_le_bytes(bytes[16..24].try_into()?),
        u64::from_le_bytes(bytes[24..32].try_into()?),
    ]))
}

#[cfg(test)]
mod tests {

    use super::*;
    use group::Group;
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
        let (x, y) = get_coordinates(point);
        println!("{:?}", (x, y));
    }

    // Testing conversion from BLS12-381 base field to Jubjub base field
    // TODO: Add randomness to val
    #[test]
    fn test_jubjub_ase_to_scalar() {
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

    // TODO: Change the errors
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
}
