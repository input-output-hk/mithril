
pub use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar,
    JubjubExtended as Jubjub, JubjubExtended, JubjubSubgroup
};
use midnight_circuits::{
    instructions::{
        HashToCurveCPU,
        hash::HashCPU,
    },
};

use ff::Field;
use group::Group;
use rand_core::{CryptoRng, RngCore};
use subtle::CtOption;
use thiserror::Error;

use crate::{error::MultiSignatureError, schnorr_signatures::helper::{get_coordinates, is_on_curve, jubjub_base_to_scalar}};
use crate::schnorr_signatures::verification_key::*;
use crate::schnorr_signatures::signature::*;

use crate::schnorr_signatures::{JubjubHashToCurve, SignatureError, PoseidonHash, DST_SIGNATURE};



/// The signing key is a scalar from the Jubjub scalar field
#[derive(Debug, Clone)]
pub struct SchnorrSigningKey(JubjubScalar);

/// Implementation of the Schnorr signature scheme using the Jubjub curve
impl SchnorrSigningKey {
    pub fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let sk = JubjubScalar::random(rng);
        SchnorrSigningKey(sk)
    }

    /// A slightly modified version of the regular Schnorr signature (I think)
    /// We include the computation of sigma, a value depending only on the msg 
    /// and the secret key as it is used for the lottery process
    pub fn sign(&self, msg: JubjubBase, rng: &mut (impl RngCore + CryptoRng)) -> SchnorrSignature {
        let g = JubjubSubgroup::generator();
        let vk = &g * &self.0;

        let hash = JubjubHashToCurve::hash_to_curve(&[msg]);
        let sigma = &hash * &self.0;
        let r = JubjubScalar::random(rng);
        let cap_r_1 = &hash * &r;
        let cap_r_2 = &g * &r;

        let (hx, hy) = get_coordinates(hash);
        let (vk_x, vk_y) = get_coordinates(vk);
        let (sigma_x, sigma_y) = get_coordinates(sigma);
        let (cap_r_1_x, cap_r_1_y) = get_coordinates(cap_r_1);
        let (cap_r_2_x, cap_r_2_y) = get_coordinates(cap_r_2);

        let c = PoseidonHash::hash(&[
            DST_SIGNATURE,
            hx,
            hy,
            vk_x,
            vk_y,
            sigma_x,
            sigma_y,
            cap_r_1_x,
            cap_r_1_y,
            cap_r_2_x,
            cap_r_2_y,
        ]);
        let c_scalar = jubjub_base_to_scalar(c);
        let s = r - self.0 * c_scalar;

        SchnorrSignature { sigma, s, c }
    }

    /// Convert the schnorr secret key into byte string.
    /// Uses midnight curve implem for the conversion
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SchnorrSigningKey`.
    ///
    /// # Error
    /// Fails if the byte string represents a scalar larger than the group order.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        // This is a bit ugly, I'll try to find a better way to do it
        let bytes = bytes.get(..32).ok_or(MultiSignatureError::SerializationError)?.try_into().unwrap();
        // Jubjub returs a CtChoice so I convert it to an option that looses the const time property
        match JubjubScalar::from_bytes(bytes).into_option().ok_or(MultiSignatureError::SerializationError) {
            Ok(sk) => Ok(Self(sk)),
            // the error should be updated
            Err(e) => Err(e)
        }
    }

}

// Should we have this implementation?
impl From<&SchnorrSigningKey> for SchnorrVerificationKey {
    fn from(sk: &SchnorrSigningKey) -> Self {
        let g = JubjubSubgroup::generator();
        let vk = &g * &sk.0;
        SchnorrVerificationKey(vk)
    }
}