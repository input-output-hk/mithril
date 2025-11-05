use midnight_circuits::instructions::{HashToCurveCPU, hash::HashCPU};

pub use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};

use group::{Group, GroupEncoding};

use crate::error::MultiSignatureError;
use crate::schnorr_signatures::helper::{get_coordinates, jubjub_base_to_scalar};
use crate::schnorr_signatures::{hash_msg_to_base, verification_key::*};
use crate::schnorr_signatures::{DST_SIGNATURE, JubjubHashToCurve, PoseidonHash, SignatureError};

/// Schnorr signature including the value sigma used for the lottery
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchnorrSignature {
    pub sigma: JubjubSubgroup,
    pub s: JubjubScalar,
    pub c: JubjubBase,
}

impl SchnorrSignature {
    /// Verify a signature against a verification key.
    pub fn verify(
        &self,
        msg: &[u8],
        vk: &SchnorrVerificationKey,
    ) -> Result<(), SignatureError> {

        let g = JubjubSubgroup::generator();
        let hash = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_base(msg)]);
        let c_scalar = jubjub_base_to_scalar(self.c);

        let (hx, hy) = get_coordinates(hash);
        let (vk_x, vk_y) = get_coordinates(vk.0);
        let (sigma_x, sigma_y) = get_coordinates(self.sigma);
        let cap_r_1_prime = hash * self.s + self.sigma * c_scalar;
        let cap_r_2_prime = g * self.s + vk.0 * c_scalar;
        let (cap_r_1_x_prime, cap_r_1_y_prime) = get_coordinates(cap_r_1_prime);
        let (cap_r_2_x_prime, cap_r_2_y_prime) = get_coordinates(cap_r_2_prime);

        let c_prime = PoseidonHash::hash(&[
            DST_SIGNATURE,
            hx,
            hy,
            vk_x,
            vk_y,
            sigma_x,
            sigma_y,
            cap_r_1_x_prime,
            cap_r_1_y_prime,
            cap_r_2_x_prime,
            cap_r_2_y_prime,
        ]);

        if c_prime != self.c {
            return Err(SignatureError::VerificationFailed);
        }

        Ok(())
    }

    // pub fn sigma(&self) -> (JubjubBase, JubjubBase) {
    //     let (x, y) = get_coordinates(self.sigma);
    //     (x, y)
    // }

    /// Convert an `SchnorrSignature` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        let mut out = [0;96]; 
        out[0..32].copy_from_slice(&self.sigma.to_bytes());
        out[32..64].copy_from_slice(&self.s.to_bytes());
        out[64..96].copy_from_slice(&self.c.to_bytes_le());
        out
    }

    /// Convert a string of bytes into a `SchnorrSignature`.
    /// Not happy with the current CtChoice handling, have to looking into cleaner way to do it
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let bytes = bytes.get(..96).ok_or(MultiSignatureError::SerializationError)?;
        let sigma = JubjubSubgroup::from_bytes(&bytes[0..32].try_into().unwrap())
            .into_option()
            .ok_or(MultiSignatureError::SerializationError);
        let s = JubjubScalar::from_bytes(&bytes[32..64].try_into().unwrap())
            .into_option()
            .ok_or(MultiSignatureError::SerializationError);
        let c = JubjubBase::from_bytes_le(&bytes[64..96].try_into().unwrap())
            .into_option()
            .ok_or(MultiSignatureError::SerializationError);
        match (sigma, s, c) {
            (Ok(sigma),Ok(s), Ok(c)) => Ok(Self{sigma,s,c}),
            _ => Err(MultiSignatureError::SerializationError)
        }
    }

}
