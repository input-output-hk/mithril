#![allow(dead_code)]

use midnight_circuits::{
    ecc::{hash_to_curve::HashToCurveGadget, native::EccChip},
    hash::poseidon::PoseidonChip,
    types::AssignedNative,
};
use midnight_curves::{Fq as JubjubBase, JubjubAffine, JubjubExtended, JubjubSubgroup};
use sha2::{Digest, Sha256};

mod signature;
mod signing_key;
mod verification_key;

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
pub(crate) fn hash_msg_to_jubjubbase(msg: &[u8]) -> JubjubBase {
    let mut hash = Sha256::new();
    hash.update(msg);
    let hmsg = hash.finalize();
    let mut output = [0u8; 32];
    output.copy_from_slice(&hmsg);
    JubjubBase::from_raw([
        u64::from_le_bytes(output[0..8].try_into().unwrap()),
        u64::from_le_bytes(output[8..16].try_into().unwrap()),
        u64::from_le_bytes(output[16..24].try_into().unwrap()),
        u64::from_le_bytes(output[24..32].try_into().unwrap()),
    ])
}

pub(crate) fn get_coordinates(point: JubjubSubgroup) -> (JubjubBase, JubjubBase) {
    let extended = JubjubExtended::from(point); // Convert to JubjubExtended
    let affine = JubjubAffine::from(extended); // Convert to JubjubAffine (affine coordinates)
    let x = affine.get_u(); // Get x-coordinate
    let y = affine.get_v(); // Get y-coordinate
    (x, y)
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
        let h = hash_msg_to_jubjubbase(&msg);
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

    // TODO: Complete the test once the verification function is implemented
    #[test]
    fn test_sig() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let _vk = SchnorrVerificationKey::from(&sk);
        let _sig = sk.sign(&msg, &mut rng);
    }
}
