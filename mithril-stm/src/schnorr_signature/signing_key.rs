use anyhow::anyhow;
use dusk_jubjub::{
    ExtendedPoint as JubjubExtended, Fr as JubjubScalar, SubgroupPoint as JubjubSubgroup,
};
use dusk_poseidon::{Domain, Hash};
use ff::Field;
use rand_core::{CryptoRng, RngCore};

use group::Group;

use crate::{
    StmResult,
    error::SchnorrSignatureError,
    schnorr_signature::{
        DST_SIGNATURE, SchnorrSignature, SchnorrVerificationKey, get_coordinates_extended,
        get_coordinates_subgroup,
    },
};

/// Schnorr Signing key, it is essentially a random scalar of the Jubjub scalar field
#[derive(Debug, Clone)]
pub struct SchnorrSigningKey(pub JubjubScalar);

impl SchnorrSigningKey {
    /// Generate a random scalar value to use as signing key
    pub fn generate(rng: &mut (impl RngCore + CryptoRng)) -> Self {
        SchnorrSigningKey(JubjubScalar::random(rng))
    }

    /// This function is an adapted version of the Schnorr signature scheme
    /// and works with the Jubjub elliptic curve and the Poseidon hash function.
    ///
    /// Input:
    ///     - a message: some bytes
    ///     - a secret key: a element of the scalar field of the Jubjub curve
    /// Output:
    ///     - a signature of the form (sigma, signature, challenge) where sigma is deterministic based
    /// on the message and the secret key and the signature and challenge are computed using randomness
    ///
    /// The protocol computes:
    ///     - sigma = H(Sha256(msg)) * secret_key
    ///     - random_scalar, a random value
    ///     - random_point_1 = H(Sha256(msg)) * random_scalar
    ///     - random_point_2 = generator * random_scalar, where generator is a generator of the prime-order subgroup of Jubjub
    ///     - challenge = Poseidon(DST || H(Sha256(msg)) || verification_key || sigma || random_point_1 || random_point_2)
    ///     - signature = random_scalar - challenge * signing_key
    ///
    /// Output the signature (sigma, signature, challenge)
    ///
    pub fn sign(
        &self,
        msg: &[u8],
        rng: &mut (impl RngCore + CryptoRng),
    ) -> StmResult<SchnorrSignature> {
        // Use the subgroup generator to compute the curve points
        let generator = JubjubSubgroup::generator();
        let verification_key = SchnorrVerificationKey::from(self);

        // First hashing the message to a scalar then hashing it to a curve point
        let msg_hash = JubjubExtended::hash_to_point(msg);

        let sigma = msg_hash * self.0;

        // r1 = H(msg) * r, r2 = g * r
        let random_scalar = JubjubScalar::random(rng);
        let random_point_1 = msg_hash * random_scalar;
        let random_point_2 = generator * random_scalar;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        let (msg_hash_x, msg_hash_y) = get_coordinates_extended(msg_hash);
        let (verification_key_x, verification_key_y) = get_coordinates_subgroup(verification_key.0);
        let (sigma_x, sigma_y) = get_coordinates_extended(sigma);
        let (random_point_1_x, random_point_1_y) = get_coordinates_extended(random_point_1);
        let (random_point_2_x, random_point_2_y) = get_coordinates_subgroup(random_point_2);

        let challenge = Hash::digest_truncated(
            Domain::Other,
            &[
                DST_SIGNATURE,
                msg_hash_x,
                msg_hash_y,
                verification_key_x,
                verification_key_y,
                sigma_x,
                sigma_y,
                random_point_1_x,
                random_point_1_y,
                random_point_2_x,
                random_point_2_y,
            ],
        )[0];

        let signature = random_scalar - challenge * self.0;

        Ok(SchnorrSignature {
            sigma,
            signature,
            challenge,
        })
    }

    /// Convert a `SchnorrSigningKey` into a string of bytes.
    pub(crate) fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert a string of bytes into a `SchnorrSigningKey`.
    ///
    /// The bytes must represent a Jubjub scalar or the conversion will fail
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let mut signing_key_bytes: [u8; 32] = [0u8; 32];
        signing_key_bytes.copy_from_slice(
            bytes
                .get(0..32)
                .ok_or(anyhow!(SchnorrSignatureError::SerializationError))?,
        );

        // Jubjub returs a CtChoice so I convert it to an option that looses the const time property
        match JubjubScalar::from_bytes(&signing_key_bytes).into_option() {
            Some(signing_key) => Ok(Self(signing_key)),
            None => Err(anyhow!(SchnorrSignatureError::SerializationError)),
        }
    }
}

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
