use anyhow::{Result, anyhow};
use ff::Field;
use midnight_circuits::hash::poseidon::PoseidonChip;
use midnight_circuits::instructions::hash::HashCPU;
use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};
use rand_core::{CryptoRng, RngCore};

use midnight_circuits::instructions::HashToCurveCPU;

use group::Group;

use crate::schnorr_signature::{
    DST_SIGNATURE, JubjubHashToCurve, get_coordinates, hash_msg_to_jubjubbase,
    jubjub_base_to_scalar,
};
use crate::schnorr_signature::{
    signature::SchnorrSignature, verification_key::SchnorrVerificationKey,
};

/// Schnorr Signing key, it is essentially a random scalar of the Jubjub scalar field
#[derive(Debug, Clone)]
pub(crate) struct SchnorrSigningKey(pub(crate) JubjubScalar);

impl SchnorrSigningKey {
    pub(crate) fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        SchnorrSigningKey(JubjubScalar::random(rng))
    }

    // TODO: Check if we want the sign function to handle the randomness by itself
    pub(crate) fn sign(
        &self,
        msg: &[u8],
        rng: &mut (impl RngCore + CryptoRng),
    ) -> Result<SchnorrSignature> {
        // Use the subgroup generator to compute the curve points
        let g = JubjubSubgroup::generator();
        let vk = SchnorrVerificationKey::from(self);

        // First hashing the message to a scalar then hashing it to a curve point
        let hash = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_jubjubbase(msg)?]);

        // sigma = H(msg) * sk
        let sigma = hash * self.0;

        // Compute the random part of the signature with
        // r1 = H(msg) * r
        // r2 = g * r
        let r = JubjubScalar::random(rng);
        let r1 = hash * r;
        let r2 = g * r;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        // I use gx and gy for now but maybe we can replace them by a DST?
        let (hashx, hashy) = get_coordinates(hash);
        let (vkx, vky) = get_coordinates(vk.0);
        let (sigmax, sigmay) = get_coordinates(sigma);
        let (r1x, r1y) = get_coordinates(r1);
        let (r2x, r2y) = get_coordinates(r2);

        let c = PoseidonChip::<JubjubBase>::hash(&[
            DST_SIGNATURE,
            hashx,
            hashy,
            vkx,
            vky,
            sigmax,
            sigmay,
            r1x,
            r1y,
            r2x,
            r2y,
        ]);

        // We want to use the from_raw function because the result of
        // the poseidon hash might not fit into the smaller modulus
        // the Fr scalar field
        // TODO: Refactor this
        let c_scalar = jubjub_base_to_scalar(&c)?;
        let s = r - c_scalar * self.0;

        Ok(SchnorrSignature { sigma, s, c })
    }

    fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SchnorrSigningKey`.
    /// The bytes must represent a Jubjub scalar or the conversion will fail
    // TODO: Maybe rework this function, do we want to allow any bytes representation
    // to be convertible to a sk?
    fn from_bytes(bytes: &[u8]) -> Result<Self> {
        // This is a bit ugly, I'll try to find a better way to do it
        let bytes = bytes
            .get(..32)
            .ok_or(anyhow!("Not enough bytes to create a signing key."))?
            .try_into()?;
        // Jubjub returs a CtChoice so I convert it to an option that looses the const time property
        match JubjubScalar::from_bytes(bytes).into_option() {
            Some(sk) => Ok(Self(sk)),
            // the error should be updated
            None => Err(anyhow!(
                "Failed to create a Jubjub scalar from the given bytes."
            )),
        }
    }
}

//
#[cfg(test)]
mod tests {
    use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    impl PartialEq for SchnorrSigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }

    #[test]
    fn test_generate_signing_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let _sk = SchnorrSigningKey::generate(&mut rng);
    }

    #[test]
    fn test_to_from_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::generate(&mut rng);
        let bytes = sk.to_bytes();
        let recovered_sk = SchnorrSigningKey::from_bytes(&bytes).unwrap();
        assert_eq!(sk, recovered_sk);
    }

    // For now failing test, maybe change it later depending on what
    // we want for from_bytes
    #[test]
    fn failing_test_from_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let mut sk = [0; 32];
        rng.fill_bytes(&mut sk);
        // Setting the msb to 1 to make sk bigger than the modulus
        sk[0] |= 0xff;
        let result = SchnorrSigningKey::from_bytes(&sk);

        assert!(
            result.is_err(),
            "Value is not a proper sk, test should fail."
        );
    }
}
