use anyhow::{Result, anyhow};
use ff::Field;
use midnight_circuits::{
    hash::poseidon::PoseidonChip,
    instructions::{HashToCurveCPU, hash::HashCPU},
};
use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};
use rand_core::{CryptoRng, RngCore};

use group::Group;

use crate::schnorr_signature::{
    DST_SIGNATURE, JubjubHashToCurve, SchnorrSignature, SchnorrVerificationKey,
    utils::{get_coordinates, hash_msg_to_jubjubbase, jubjub_base_to_scalar},
};

/// Schnorr Signing key, it is essentially a random scalar of the Jubjub scalar field
#[derive(Debug, Clone)]
pub struct SchnorrSigningKey(pub(crate) JubjubScalar);

impl SchnorrSigningKey {
    pub(crate) fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        SchnorrSigningKey(JubjubScalar::random(rng))
    }

    /// This function is an adapted version of the Schnorr signature scheme
    /// and works with the Jubjub elliptic curve and the Poseidon hash function.
    ///
    /// The scheme works as follows:
    /// Input:
    ///     - a message: some bytes
    ///     - a secret key: a element of the scalar field of the Jubjub curve
    ///     - a verification key: a value depending on the secret key
    /// Output:
    ///     - a signature of the form (sigma, signature, challenge) where sigma is deterministic based
    /// on the message and the secret key and the signature and challenge are computed using randomness
    ///
    /// The protocol:
    ///
    /// The signature follows closely the regular Schnorr signature computation but uses elliptic curve
    /// based computation (without pairings). The first step is the convert the message bytes into a point
    /// on the elliptic curve. This is done by hashing the bytes, using for example Sha2, to a 256 bits value
    /// that can be converted to a scalar of the BLS12-381 field. This scalar is converted to a point on the
    /// elliptic curve using the `hash_to_curve` function. This allows to compute the deterministic value
    /// sigma = H(Sha256(msg)) * secret_key which is needed later in the Mithril protocol.
    ///
    /// The rest of the protocol follows the regular Schnorr signature:
    ///     - Choose a random value r, here is the scalar field of Jubjub
    ///     - Compute two values based on r:
    ///         - R1 = H(Sha256(msg)) * r
    ///         - R2 = g * r, where g is a generator of the prime-order subgroup of Jubjub
    ///     - Compute the challenge, that does not depend on sk, as:
    ///         - challenge = Poseidon(DST || H(Sha256(msg)) || vk || sigma || R1 || R2), since the Poseidon
    /// hash function takes as input element of the scalar field of BLS21-381, we need to convert
    /// the elliptic curve points to their coordinates representation to feed them to the hash function.
    ///     - Convert the challenge into an element of the scalar field of Jubjub and compute:
    ///         - signature = r - challenge * sk
    ///     - Output the signature (sigma, signature, challenge)
    ///
    /// The verification algorithm consists in recomputing the challenge from the signature value and
    /// checking it matches the challenge value in the Schnorr signature. It is described in more
    /// details in the implementation of the SchnorrSignature.
    ///
    // TODO: Check if we want the sign function to handle the randomness by itself
    pub(crate) fn sign(
        &self,
        msg: &[u8],
        rng: &mut (impl RngCore + CryptoRng),
    ) -> Result<SchnorrSignature> {
        // Use the subgroup generator to compute the curve points
        let generator = JubjubSubgroup::generator();
        let verification_key = SchnorrVerificationKey::from(self);

        // First hashing the message to a scalar then hashing it to a curve point
        let hash_msg = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_jubjubbase(msg)?]);

        // sigma = H(Sha256(msg)) * sk
        let sigma = hash_msg * self.0;

        // Compute the random part of the signature with
        // r1 = H(msg) * r
        // r2 = g * r
        let random_scalar = JubjubScalar::random(rng);
        let random_value_1 = hash_msg * random_scalar;
        let random_value_2 = generator * random_scalar;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        // I use gx and gy for now but maybe we can replace them by a DST?
        let (hash_msg_x, hash_msg_y) = get_coordinates(hash_msg);
        let (verification_key_x, verification_key_y) = get_coordinates(verification_key.0);
        let (sigma_x, sigma_y) = get_coordinates(sigma);
        let (random_value_1_x, random_value_1_y) = get_coordinates(random_value_1);
        let (random_value_2_x, random_value_2_y) = get_coordinates(random_value_2);

        let challenge = PoseidonChip::<JubjubBase>::hash(&[
            DST_SIGNATURE,
            hash_msg_x,
            hash_msg_y,
            verification_key_x,
            verification_key_y,
            sigma_x,
            sigma_y,
            random_value_1_x,
            random_value_1_y,
            random_value_2_x,
            random_value_2_y,
        ]);

        // We want to use the from_raw function because the result of
        // the poseidon hash might not fit into the smaller modulus
        // the Fr scalar field
        let challenge_scalar = jubjub_base_to_scalar(&challenge)?;
        let signature = random_scalar - challenge_scalar * self.0;

        Ok(SchnorrSignature {
            sigma,
            signature,
            challenge,
        })
    }

    pub(crate) fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SchnorrSigningKey`.
    ///
    /// The bytes must represent a Jubjub scalar or the conversion will fail
    // TODO: Maybe rework this function, do we want to allow any bytes representation
    // to be convertible to a sk?
    pub(crate) fn from_bytes(bytes: &[u8]) -> Result<Self> {
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
    pub(crate) use super::*;
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
