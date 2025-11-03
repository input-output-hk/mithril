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

pub use midnight_curves::{
    Bls12, EDWARDS_D, Fq as JubjubBase, Fq as BlsScalar, Fr as JubjubScalar,
    G1Affine as BlstG1Affine, G1Projective as BlstG1, G2Affine as BlstG2Affine, JubjubAffine,
    JubjubExtended as Jubjub, JubjubExtended, JubjubSubgroup, MODULUS,
};



use ff::Field;
use group::Group;
use rand_core::{CryptoRng, RngCore};
use subtle::{Choice, ConstantTimeEq};
use thiserror::Error;

// use crate::schnorr_signatures::{get_coordinates, jubjub_base_to_scalar, is_on_curve};



pub fn get_coordinates(point: JubjubSubgroup) -> (JubjubBase, JubjubBase) {
    let extended: JubjubExtended = point.into(); // Convert to JubjubExtended
    let affine: JubjubAffine = extended.into(); // Convert to JubjubAffine (affine coordinates)
    let x = affine.get_u(); // Get x-coordinate
    let y = affine.get_v(); // Get y-coordinate
    (x, y)
}

pub fn jubjub_base_to_scalar(x: JubjubBase) -> JubjubScalar {
    let bytes = x.to_bytes_le();
    JubjubScalar::from_raw([
        u64::from_le_bytes(bytes[0..8].try_into().unwrap()),
        u64::from_le_bytes(bytes[8..16].try_into().unwrap()),
        u64::from_le_bytes(bytes[16..24].try_into().unwrap()),
        u64::from_le_bytes(bytes[24..32].try_into().unwrap()),
    ])
}


pub fn is_on_curve(u: JubjubBase, v: JubjubBase) -> Choice {
    let u2 = u.square();
    let v2 = v.square();

    // Left-hand side: v² - u²
    let lhs = v2 - u2;

    // Right-hand side: 1 + EDWARDS_D * (u² * v²)
    let rhs = JubjubBase::ONE + EDWARDS_D * u2 * v2;

    // Compare in constant time
    lhs.ct_eq(&rhs)
}
