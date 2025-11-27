use anyhow::{Context, anyhow};
use dusk_jubjub::{
    ExtendedPoint as JubjubExtended, Fq as JubjubBase, Fr as JubjubScalar,
    SubgroupPoint as JubjubSubgroup,
};
use dusk_poseidon::{Domain, Hash};
use group::Group;
use rand_core::{CryptoRng, RngCore};

use crate::{
    StmResult,
    signature_scheme::{
        SchnorrSignature, SchnorrSignatureError, SchnorrVerificationKey, generate_non_zero_scalar,
        get_coordinates_several_points, schnorr_signature::DST_SIGNATURE,
    },
};

/// Schnorr Signing key, it is essentially a random scalar of the Jubjub scalar field
#[derive(Debug, Clone)]
pub struct SchnorrSigningKey(pub(crate) JubjubScalar);

impl SchnorrSigningKey {
    /// Generate a random scalar value to use as signing key
    pub fn try_generate<R: RngCore + CryptoRng>(rng: &mut R) -> StmResult<Self> {
        let r = generate_non_zero_scalar(rng);
        match r {
            Ok(s) => Ok(SchnorrSigningKey(s)),
            Err(_) => Err(anyhow!(SchnorrSignatureError::SigningKeyGenerationError))
                .with_context(|| "Failed to generate a non zero schnorr signing key."),
        }
    }

    /// This function is an adapted version of the Schnorr signature scheme that includes
    /// the computation of a deterministic value (called sigma) based on the message and the signing key
    /// and works with the Jubjub elliptic curve and the Poseidon hash function.
    ///
    /// Input:
    ///     - a message: some bytes
    ///     - a secret key: an element of the scalar field of the Jubjub curve
    /// Output:
    ///     - a signature of the form (sigma, signature, challenge):
    ///         - sigma is deterministic depending only on the message and secret key
    ///         - the signature and challenge depends on a random value generated during the signature
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
    pub fn sign<R: RngCore + CryptoRng>(
        &self,
        msg: &[u8],
        rng: &mut R,
    ) -> StmResult<SchnorrSignature> {
        // Use the subgroup generator to compute the curve points
        let generator = JubjubSubgroup::generator();
        let verification_key = SchnorrVerificationKey::from(self);

        // First hashing the message to a scalar then hashing it to a curve point
        let msg_hash = JubjubExtended::hash_to_point(msg);

        let sigma = msg_hash * self.0;

        // r1 = H(msg) * r, r2 = g * r
        let random_scalar = generate_non_zero_scalar(rng)
            .with_context(|| "Failed to generate a non zero scalar for the signature.")?;

        let random_point_1 = msg_hash * random_scalar;
        let random_point_2 = generator * random_scalar;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        // The order must be preserved
        let points_coordinates = get_coordinates_several_points(&[
            msg_hash,
            verification_key.0.into(),
            sigma,
            random_point_1,
            random_point_2.into(),
        ]);

        let mut poseidon_input = vec![DST_SIGNATURE];
        poseidon_input.extend(
            points_coordinates
                .into_iter()
                .flat_map(|(x, y)| [x, y])
                .collect::<Vec<JubjubBase>>(),
        );
        let challenge = Hash::digest_truncated(Domain::Other, &poseidon_input)[0];
        let signature = random_scalar - challenge * self.0;

        Ok(SchnorrSignature {
            sigma,
            signature,
            challenge,
        })
    }

    /// Convert a `SchnorrSigningKey` into bytes.
    pub fn to_bytes(&self) -> [u8; 32] {
        self.0.to_bytes()
    }

    /// Convert bytes into a `SchnorrSigningKey`.
    ///
    /// The bytes must represent a Jubjub scalar or the conversion will fail
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 32 {
            return Err(anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Not enough bytes provided to create a Schnorr signing key.");
        }

        let signing_key_bytes = bytes[0..32]
            .try_into()
            .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to obtain the Schnorr signing key's bytes.")?;

        match JubjubScalar::from_bytes(&signing_key_bytes).into_option() {
            Some(signing_key) => Ok(Self(signing_key)),
            None => Err(anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Failed to generate a Jubjub scalar from the given bytes."),
        }
    }
}

#[cfg(test)]
mod tests {
    pub(crate) use super::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    #[test]
    fn generate_signing_key() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let _sk = SchnorrSigningKey::try_generate(&mut rng);
    }

    #[test]
    fn sign_and_verify() {
        let msg = vec![0, 0, 0, 1];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);

        let sig = sk.sign(&msg, &mut rng).unwrap();

        sig.verify(&msg, &vk).unwrap();
    }

    #[test]
    fn to_from_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();

        let sk_bytes = sk.to_bytes();
        let recovered_sk = SchnorrSigningKey::from_bytes(&sk_bytes).unwrap();

        assert_eq!(sk.0, recovered_sk.0);
    }

    // Failing test as the generated bytes represent a value too big to be converted
    // in this manner (greater than the jubjub modulus)
    #[test]
    fn failing_test_from_bytes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let mut sk = [0; 32];
        rng.fill_bytes(&mut sk);
        // Setting the msb to 1 to make sk bigger than the modulus
        sk[0] |= 0xff;

        let result = SchnorrSigningKey::from_bytes(&sk);

        result.expect_err("Value is not a proper sk, test should fail.");
    }

    #[test]
    fn from_bytes_signing_key_not_enough_bytes() {
        let msg = vec![0u8; 31];

        let result = SchnorrSigningKey::from_bytes(&msg);

        result.expect_err("Not enough bytes.");
    }

    mod golden {

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::SchnorrSigningKey;

        const GOLDEN_BYTES: &[u8; 32] = &[
            126, 191, 239, 197, 88, 151, 248, 254, 187, 143, 86, 35, 29, 62, 90, 13, 196, 71, 234,
            5, 90, 124, 205, 194, 51, 192, 228, 133, 25, 140, 157, 7,
        ];

        fn golden_value() -> SchnorrSigningKey {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            SchnorrSigningKey::try_generate(&mut rng).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = SchnorrSigningKey::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value().0, value.0);

            let serialized = SchnorrSigningKey::to_bytes(&value);
            let golden_serialized = SchnorrSigningKey::to_bytes(&golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }
}
