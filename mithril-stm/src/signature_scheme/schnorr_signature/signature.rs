use anyhow::{Context, anyhow};
use dusk_jubjub::{
    ExtendedPoint as JubjubExtended, Fq as JubjubBase, Fr as JubjubScalar,
    SubgroupPoint as JubjubSubgroup,
};
use dusk_poseidon::{Domain, Hash};
use group::{Group, GroupEncoding};

use crate::StmResult;

use super::{
    DST_SIGNATURE, SchnorrSignatureError, SchnorrVerificationKey, get_coordinates_several_points,
    is_on_curve,
};

/// Structure of the Schnorr signature to use with the SNARK
///
/// This signature includes a value `sigma` which depends only on
/// the message and the signing key.
/// This value is used in the lottery process to determine the correct indices.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SchnorrSignature {
    /// Deterministic value depending on the message and secret key
    pub(crate) sigma: JubjubExtended,
    /// Part of the Schnorr signature depending on the secret key
    pub(crate) signature: JubjubScalar,
    /// Part of the Schnorr signature NOT depending on the secret key
    pub(crate) challenge: JubjubScalar,
}

impl SchnorrSignature {
    /// This function performs the verification of a Schnorr signature given the signature, the signed message
    /// and a verification key derived from the secret key used to sign.
    ///
    /// Input:
    ///     - a Schnorr signature
    ///     - a message: some bytes
    ///     - a verification key: a value depending on the secret key
    /// Output:
    ///     - Ok(()) if the signature verifies and an error if not
    ///
    /// The protocol computes:
    ///     - msg_hash = H(Sha256(msg))
    ///     - random_point_1_recomputed = msg_hash * signature + sigma * challenge
    ///     - random_point_2_recomputed = generator * signature + verification_key * challenge
    ///     - challenge_recomputed = Poseidon(DST || H(Sha256(msg)) || verification_key
    ///     || sigma || random_point_1_recomputed || random_point_2_recomputed)
    ///
    /// Check: challenge == challenge_recomputed
    ///
    pub fn verify(&self, msg: &[u8], verification_key: &SchnorrVerificationKey) -> StmResult<()> {
        // Check that the verification key is on the curve
        if !is_on_curve(verification_key.0.into()) {
            return Err(anyhow!(SchnorrSignatureError::VerificationKeyInvalid(
                Box::new(*verification_key)
            )));
        }

        let generator = JubjubSubgroup::generator();

        // First hashing the message to a scalar then hashing it to a curve point
        let msg_hash = JubjubExtended::hash_to_point(msg);

        // Computing R1 = H(msg) * s + sigma * c
        let msg_hash_times_signature = msg_hash * self.signature;
        let sigma_times_challenge = self.sigma * self.challenge;
        let random_point_1_recomputed = msg_hash_times_signature + sigma_times_challenge;

        // Computing R2 = g * s + vk * c
        let generator_times_signature = generator * self.signature;
        let vk_times_challenge = verification_key.0 * self.challenge;
        let random_point_2_recomputed = generator_times_signature + vk_times_challenge;

        // Since the hash function takes as input scalar elements
        // We need to convert the EC points to their coordinates
        let points_coordinates = get_coordinates_several_points(&[
            msg_hash,
            verification_key.0.into(),
            self.sigma,
            random_point_1_recomputed,
            random_point_2_recomputed.into(),
        ]);

        let mut poseidon_input = vec![DST_SIGNATURE];
        poseidon_input.extend(
            points_coordinates
                .into_iter()
                .flat_map(|(x, y)| [x, y])
                .collect::<Vec<JubjubBase>>(),
        );

        let challenge_recomputed = Hash::digest_truncated(Domain::Other, &poseidon_input)[0];

        if challenge_recomputed != self.challenge {
            return Err(anyhow!(SchnorrSignatureError::SignatureInvalid(Box::new(
                *self
            ))));
        }

        Ok(())
    }

    /// Convert an `SchnorrSignature` into bytes.
    pub fn to_bytes(self) -> [u8; 96] {
        let mut out = [0; 96];
        out[0..32].copy_from_slice(&self.sigma.to_bytes());
        out[32..64].copy_from_slice(&self.signature.to_bytes());
        out[64..96].copy_from_slice(&self.challenge.to_bytes());

        out
    }

    /// Convert bytes into a `SchnorrSignature`.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 96 {
            return Err(anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Not enough bytes provided to create a signature.");
        }

        let sigma_bytes = bytes[0..32]
            .try_into()
            .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to obtain sigma's bytes.")?;
        let sigma = JubjubExtended::from_bytes(&sigma_bytes)
            .into_option()
            .ok_or(anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Unable to convert bytes into a sigma value.")?;

        let signature_bytes = bytes[32..64]
            .try_into()
            .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to obtain signature's bytes.")?;
        let signature = JubjubScalar::from_bytes(&signature_bytes)
            .into_option()
            .ok_or(anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Unable to convert bytes into a signature value.")?;

        let challenge_bytes = bytes[64..96]
            .try_into()
            .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to obtain challenge's bytes.")?;
        let challenge = JubjubScalar::from_bytes(&challenge_bytes)
            .into_option()
            .ok_or(anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Unable to convert bytes into a challenge value.")?;

        Ok(Self {
            sigma,
            signature,
            challenge,
        })
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{SchnorrSignature, SchnorrSigningKey, SchnorrVerificationKey};

    #[test]
    fn invalid_sig() {
        let msg = vec![0, 0, 0, 1];
        let msg2 = vec![0, 0, 0, 2];
        let seed = [0u8; 32];
        let mut rng = ChaCha20Rng::from_seed(seed);
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let vk = SchnorrVerificationKey::from(&sk);
        let sk2 = SchnorrSigningKey::try_generate(&mut rng).unwrap();
        let vk2 = SchnorrVerificationKey::from(&sk2);

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig2 = sk.sign(&msg2, &mut rng).unwrap();

        // Wrong verification key is used
        let result1 = sig.verify(&msg, &vk2);
        let result2 = sig2.verify(&msg, &vk);

        result1.expect_err("Wrong verification key used, test should fail.");
        // Wrong message is verified
        result2.expect_err("Wrong message used, test should fail.");
    }

    #[test]
    fn serialize_deserialize_signature() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

        let msg = vec![0, 0, 0, 1];
        let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();

        let sig = sk.sign(&msg, &mut rng).unwrap();
        let sig_bytes: [u8; 96] = sig.to_bytes();
        let sig2 = SchnorrSignature::from_bytes(&sig_bytes).unwrap();

        assert_eq!(sig, sig2);
    }

    #[test]
    fn from_bytes_signature_not_enough_bytes() {
        let msg = vec![0u8; 95];

        let result = SchnorrSignature::from_bytes(&msg);

        result.expect_err("Not enough bytes.");
    }

    mod golden {

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use crate::signature_scheme::{SchnorrSignature, SchnorrSigningKey};

        const GOLDEN_BYTES: &[u8; 96] = &[
            143, 53, 198, 62, 178, 1, 88, 253, 21, 92, 100, 13, 72, 180, 198, 127, 39, 175, 102,
            69, 147, 249, 244, 224, 122, 121, 248, 68, 217, 242, 158, 113, 94, 57, 200, 241, 208,
            145, 251, 8, 92, 119, 163, 38, 81, 85, 54, 36, 193, 221, 254, 242, 21, 129, 110, 161,
            142, 184, 107, 156, 100, 34, 190, 9, 200, 20, 178, 142, 61, 253, 193, 11, 5, 180, 97,
            73, 125, 88, 162, 36, 30, 177, 225, 52, 136, 21, 138, 93, 81, 23, 19, 64, 82, 78, 229,
            3,
        ];

        fn golden_value() -> SchnorrSignature {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let sk = SchnorrSigningKey::try_generate(&mut rng).unwrap();
            let msg = [0u8; 32];
            sk.sign(&msg, &mut rng).unwrap()
        }

        #[test]
        fn golden_conversions() {
            let value = SchnorrSignature::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = SchnorrSignature::to_bytes(value);
            let golden_serialized = SchnorrSignature::to_bytes(golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }
}
