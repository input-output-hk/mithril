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

pub use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar,
    JubjubExtended as Jubjub, JubjubExtended, JubjubSubgroup,
};


use ff::Field;
use group::Group;
use rand_core::{CryptoRng, RngCore};
use thiserror::Error;

use crate::schnorr_signatures::helper::{get_coordinates, jubjub_base_to_scalar, is_on_curve};
use crate::schnorr_signatures::{JubjubHashToCurve, SignatureError, PoseidonHash, DST_SIGNATURE};



#[derive(Debug, Clone, Copy, Default)]
pub struct VerificationKey(pub JubjubSubgroup);


impl VerificationKey {
    pub fn to_field(&self) -> [JubjubBase; 2] {
        let (x, y) = get_coordinates(self.0);
        [x, y]
    }

    pub fn to_bytes(&self) -> [u8; 64] {
        let (x, y) = get_coordinates(self.0);
        let mut bytes = [0u8; 64];
        bytes[0..32].copy_from_slice(&x.to_bytes_le());
        bytes[32..64].copy_from_slice(&y.to_bytes_le());
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SignatureError> {
        let bytes = bytes.get(0..64).ok_or(SignatureError::SerializationError)?;
        let mut u_bytes = [0u8; 32];
        u_bytes.copy_from_slice(&bytes[0..32]);
        let mut v_bytes = [0u8; 32];
        v_bytes.copy_from_slice(&bytes[32..64]);

        let u = JubjubBase::from_bytes_le(&u_bytes)
            .into_option()
            .ok_or(SignatureError::SerializationError)?;
        let v = JubjubBase::from_bytes_le(&v_bytes)
            .into_option()
            .ok_or(SignatureError::SerializationError)?;
        if !bool::from(is_on_curve(u, v)) {
            return Err(SignatureError::SerializationError);
        }

        let point = JubjubSubgroup::from_raw_unchecked(u, v);
        if !bool::from(JubjubExtended::from(point).is_prime_order()) {
            return Err(SignatureError::SerializationError);
        }

        Ok(VerificationKey(point))
    }
}