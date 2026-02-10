use ff::Field;
use group::Group;
use rand_core::{CryptoRng, RngCore};

use crate::circuits::halo2::constants::DST_UNIQUE_SIGNATURE;
use crate::circuits::halo2::hash::{HashCPU, HashToCurveCPU, JubjubHashToCurve, PoseidonHash};
use crate::circuits::halo2::off_circuit::error::SignatureError;
use crate::circuits::halo2::off_circuit::utils::{
    get_coordinates, is_on_curve, jubjub_base_to_scalar,
};
use crate::circuits::halo2::types::{Jubjub, JubjubBase, JubjubScalar, JubjubSubgroup};

#[derive(Debug, Clone)]
pub struct SigningKey(JubjubScalar);

impl SigningKey {
    pub fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let sk = JubjubScalar::random(rng);
        SigningKey(sk)
    }

    pub fn sign(&self, msg: &[JubjubBase], rng: &mut (impl RngCore + CryptoRng)) -> Signature {
        let g = JubjubSubgroup::generator();
        let vk = g * self.0;

        let hash = JubjubHashToCurve::hash_to_curve(msg);
        let sigma = hash * self.0;
        let r = JubjubScalar::random(rng);
        let cap_r_1 = hash * r;
        let cap_r_2 = g * r;

        let (hx, hy) = get_coordinates(hash);
        let (vk_x, vk_y) = get_coordinates(vk);
        let (sigma_x, sigma_y) = get_coordinates(sigma);
        let (cap_r_1_x, cap_r_1_y) = get_coordinates(cap_r_1);
        let (cap_r_2_x, cap_r_2_y) = get_coordinates(cap_r_2);

        let c = PoseidonHash::hash(&[
            DST_UNIQUE_SIGNATURE,
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

        Signature { sigma, s, c }
    }

}

#[derive(Debug, Clone, Copy, Default)]
pub struct VerificationKey(pub JubjubSubgroup);

impl From<&SigningKey> for VerificationKey {
    fn from(sk: &SigningKey) -> Self {
        let g = JubjubSubgroup::generator();
        let vk = g * sk.0;
        VerificationKey(vk)
    }
}

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
        if !bool::from(Jubjub::from(point).is_prime_order()) {
            return Err(SignatureError::SerializationError);
        }

        Ok(VerificationKey(point))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub sigma: JubjubSubgroup,
    pub s: JubjubScalar,
    pub c: JubjubBase,
}

impl Signature {
    /// Verify a signature against a verification key.
    pub fn verify(&self, msg: &[JubjubBase], vk: &VerificationKey) -> Result<(), SignatureError> {
        let g = JubjubSubgroup::generator();
        let hash = JubjubHashToCurve::hash_to_curve(msg);
        let c_scalar = jubjub_base_to_scalar(self.c);

        let (hx, hy) = get_coordinates(hash);
        let (vk_x, vk_y) = get_coordinates(vk.0);
        let (sigma_x, sigma_y) = get_coordinates(self.sigma);
        let cap_r_1_prime = hash * self.s + self.sigma * c_scalar;
        let cap_r_2_prime = g * self.s + vk.0 * c_scalar;
        let (cap_r_1_x_prime, cap_r_1_y_prime) = get_coordinates(cap_r_1_prime);
        let (cap_r_2_x_prime, cap_r_2_y_prime) = get_coordinates(cap_r_2_prime);
        let c_prime = PoseidonHash::hash(&[
            DST_UNIQUE_SIGNATURE,
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


#[cfg(test)]
mod tests {
    use super::*;
    use rand_core::OsRng;

    /// Test signing functionality.
    #[test]
    fn test_signature_unique_verification_valid() {
        let mut rng = OsRng;
        let sk = SigningKey::generate(&mut rng);
        let msg = JubjubBase::random(&mut rng);

        // Sign the message
        let signature = sk.sign(&[msg], &mut rng);

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

        signature.verify(&[msg], &VerificationKey::from(&sk)).unwrap();
    }

    #[test]
    fn test_signature_unique_verification_invalid_signature() {
        let mut rng = OsRng;
        let sk = SigningKey::generate(&mut rng);
        let msg = JubjubBase::random(&mut rng);
        let vk: VerificationKey = (&sk).into();

        // Generate signature and tamper with it
        let mut signature = sk.sign(&[msg], &mut rng);
        signature.s = JubjubScalar::random(&mut rng); // Modify `s` component

        // Verify the modified signature
        let result = signature.verify(&[msg], &vk);
        assert!(
            result.is_err(),
            "Invalid signature should fail verification, but it passed."
        );
    }

}
