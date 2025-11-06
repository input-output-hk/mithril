use ff::Field;
use midnight_circuits::hash::poseidon::PoseidonChip;
use midnight_circuits::instructions::hash::HashCPU;
use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};
use rand_core::{CryptoRng, RngCore};

use midnight_circuits::instructions::HashToCurveCPU;

use group::Group;

use crate::schnorr_signature::{JubjubHashToCurve, get_coordinates, hash_msg_to_jubjubbase};
use crate::schnorr_signature::{
    signature::SchnorrSignature, verification_key::SchnorrVerificationKey,
};

pub(crate) struct SchnorrSigningKey(pub(crate) JubjubScalar);

impl SchnorrSigningKey {
    pub(crate) fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        SchnorrSigningKey(JubjubScalar::random(rng))
    }

    pub(crate) fn sign(
        &self,
        msg: &[u8],
        rng: &mut (impl RngCore + CryptoRng),
    ) -> SchnorrSignature {
        // Use the subgroup generator to compute the curve points
        let g = JubjubSubgroup::generator();
        let vk = SchnorrVerificationKey::from(self);

        // First hashing the message to a scalar then hashing it to a curve point
        let hash = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_jubjubbase(msg)]);

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
        let (gx, gy) = get_coordinates(g);
        let (hashx, hashy) = get_coordinates(hash);
        let (vkx, vky) = get_coordinates(vk.0);
        let (sigmax, sigmay) = get_coordinates(sigma);
        let (r1x, r1y) = get_coordinates(r1);
        let (r2x, r2y) = get_coordinates(r2);

        let c = PoseidonChip::<JubjubBase>::hash(&[
            gx, gy, hashx, hashy, vkx, vky, sigmax, sigmay, r1x, r1y, r2x, r2y,
        ]);

        // TODO: remove the unwrap and handle the error
        let c_scalar = JubjubScalar::from_bytes(&c.to_bytes_le()).into_option().unwrap();
        let s = r - c_scalar * self.0;

        SchnorrSignature {
            sigma,
            s,
            c,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn test_generate_signing_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let _sk = SchnorrSigningKey::generate(&mut rng);
    }

    #[test]
    fn test_generate_signature() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let _sk = SchnorrSigningKey::generate(&mut rng);
    }
}
