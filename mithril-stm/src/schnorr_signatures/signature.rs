
use midnight_circuits::{
    instructions::{
        HashToCurveCPU,
        hash::HashCPU,
    },
};

pub use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar,
    JubjubSubgroup,
};


use group::Group;

use crate::schnorr_signatures::helper::{get_coordinates, jubjub_base_to_scalar, is_on_curve};
use crate::schnorr_signatures::verification_key::*;
use crate::schnorr_signatures::{JubjubHashToCurve, SignatureError, PoseidonHash, DST_SIGNATURE};



/// Schnorr signature including the value sigma used for the lottery
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SchnorrSignature {
    pub sigma: JubjubSubgroup,
    pub s: JubjubScalar,
    pub c: JubjubBase,
}

impl SchnorrSignature {
    /// Verify a signature against a verification key.
    pub fn verify(&self, msg: JubjubBase, vk: &VerificationKey) -> Result<(), SignatureError> {
        let g = JubjubSubgroup::generator();
        let hash = JubjubHashToCurve::hash_to_curve(&[msg]);
        let c_scalar = jubjub_base_to_scalar(self.c);

        let (hx, hy) = get_coordinates(hash);
        let (vk_x, vk_y) = get_coordinates(vk.0);
        let (sigma_x, sigma_y) = get_coordinates(self.sigma);
        let cap_r_1_prime = &hash * &self.s + &self.sigma * &c_scalar;
        let cap_r_2_prime = &g * &self.s + &vk.0 * &c_scalar;
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

    pub fn sigma(&self) -> (JubjubBase, JubjubBase) {
        let (x, y) = get_coordinates(self.sigma);
        (x, y)
    }
}

