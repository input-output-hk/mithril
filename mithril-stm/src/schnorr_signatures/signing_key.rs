
pub use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar,
    JubjubExtended as Jubjub, JubjubExtended, JubjubSubgroup,
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
use thiserror::Error;

use crate::schnorr_signatures::helper::{get_coordinates, jubjub_base_to_scalar, is_on_curve};
use crate::schnorr_signatures::verification_key::*;
use crate::schnorr_signatures::signature::*;

use crate::schnorr_signatures::{JubjubHashToCurve, SignatureError, PoseidonHash, DST_SIGNATURE};



/// The signing key is a scalar from the Jubjub scalar field
#[derive(Debug, Clone)]
pub struct SigningKey(JubjubScalar);

/// Implementation of the Schnorr signature scheme using the Jubjub curve
impl SigningKey {
    pub fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let sk = JubjubScalar::random(rng);
        SigningKey(sk)
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

}


impl From<&SigningKey> for VerificationKey {
    fn from(sk: &SigningKey) -> Self {
        let g = JubjubSubgroup::generator();
        let vk = &g * &sk.0;
        VerificationKey(vk)
    }
}