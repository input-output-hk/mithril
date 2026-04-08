use crate::circuits::halo2_ivc::{
    DST_SCHNORR_SIGNATURE, HashCPU, Jubjub, JubjubBase, JubjubScalar, JubjubSubgroup,
    PoseidonHash,
};
use crate::circuits::halo2_ivc::helpers::{
    signatures::SignatureError,
    utils::{get_coordinates, is_on_curve, jubjub_base_to_scalar},
};
use ff::Field;
use group::Group;
use rand_core::{CryptoRng, RngCore};

#[derive(Debug, Clone)]
pub struct SigningKey(JubjubScalar);

impl SigningKey {
    pub fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        let sk = JubjubScalar::random(rng);
        SigningKey(sk)
    }

    pub fn sign(&self, msg: &[JubjubBase], rng: &mut (impl RngCore + CryptoRng)) -> Signature {
        let g = JubjubSubgroup::generator();
        let vk = &g * &self.0;
        let r = JubjubScalar::random(rng);
        let cap_r = &g * &r;
        let (vk_x, vk_y) = get_coordinates(vk);
        let (cap_r_x, cap_r_y) = get_coordinates(cap_r);

        let mut to_hash = vec![DST_SCHNORR_SIGNATURE, vk_x, vk_y, cap_r_x, cap_r_y];
        to_hash.extend_from_slice(msg);

        let c = PoseidonHash::hash(&to_hash);
        let c_scalar = jubjub_base_to_scalar(c);
        let s = r - self.0 * c_scalar;

        Signature { s, c }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct VerificationKey(pub JubjubSubgroup);

impl From<&SigningKey> for VerificationKey {
    fn from(sk: &SigningKey) -> Self {
        let g = JubjubSubgroup::generator();
        let vk = &g * &sk.0;
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
    pub s: JubjubScalar,
    pub c: JubjubBase,
}

impl Signature {
    /// Verify a signature against a verification key.
    pub fn verify(&self, msg: &[JubjubBase], vk: &VerificationKey) -> Result<(), SignatureError> {
        let g = JubjubSubgroup::generator();
        let c_scalar = jubjub_base_to_scalar(self.c);

        let (vk_x, vk_y) = get_coordinates(vk.0);
        let cap_r_prime = &g * &self.s + &vk.0 * &c_scalar;
        let (cap_r_x_prime, cap_r_y_prime) = get_coordinates(cap_r_prime);

        let mut to_hash = vec![
            DST_SCHNORR_SIGNATURE,
            vk_x,
            vk_y,
            cap_r_x_prime,
            cap_r_y_prime,
        ];
        to_hash.extend_from_slice(msg);

        let c_prime = PoseidonHash::hash(&to_hash);
        if c_prime != self.c {
            return Err(SignatureError::VerificationFailed);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand_core::OsRng;

    /// Test signing functionality.
    #[test]
    fn test_signature_schnorr_verification_valid() {
        let mut rng = OsRng;
        let sk = SigningKey::generate(&mut rng);
        let msg = JubjubBase::random(&mut rng);

        // Sign the message
        let signature = sk.sign(&[msg], &mut rng);

        // Ensure the components of the signature are non-default values
        assert_ne!(
            signature.s,
            JubjubScalar::ZERO,
            "Schnorr signature s component should not be zero."
        );
        assert_ne!(
            signature.c,
            JubjubBase::ZERO,
            "Schnorr signature c component should not be zero."
        );

        signature
            .verify(&[msg], &VerificationKey::from(&sk))
            .unwrap();
    }

    #[test]
    fn test_signature_schnorr_verification_invalid_signature() {
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
            "Invalid Schnorr signature should fail verification, but it passed."
        );
    }
}
