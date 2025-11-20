use anyhow::{Context, anyhow};

use dusk_jubjub::{
    ExtendedPoint as JubjubExtended, Fr as JubjubScalar, SubgroupPoint as JubjubSubgroup,
};
use dusk_poseidon::{Domain, Hash};
use group::{Group, GroupEncoding};

use crate::{
    StmResult,
    error::SchnorrSignatureError,
    schnorr_signature::{
        DST_SIGNATURE, SchnorrVerificationKey, get_coordinates_extended, get_coordinates_subgroup,
    },
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
    // pub(crate) challenge: JubjubBase,
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
        let (msg_hash_x, msg_hash_y) = get_coordinates_extended(msg_hash);
        let (verification_key_x, verification_key_y) = get_coordinates_subgroup(verification_key.0);
        let (sigma_x, sigma_y) = get_coordinates_extended(self.sigma);
        let (random_point_1_recomputed_x, random_point_1_recomputed_y) =
            get_coordinates_extended(random_point_1_recomputed);
        let (random_point_2_recomputed_x, random_point_2_recomputed_y) =
            get_coordinates_subgroup(random_point_2_recomputed);

        let challenge_recomputed = Hash::digest_truncated(
            Domain::Other,
            &[
                DST_SIGNATURE,
                msg_hash_x,
                msg_hash_y,
                verification_key_x,
                verification_key_y,
                sigma_x,
                sigma_y,
                random_point_1_recomputed_x,
                random_point_1_recomputed_y,
                random_point_2_recomputed_x,
                random_point_2_recomputed_y,
            ],
        )[0];

        if challenge_recomputed != self.challenge {
            return Err(anyhow!(SchnorrSignatureError::SignatureInvalid(Box::new(
                *self
            ))));
        }

        Ok(())
    }

    /// Convert an `SchnorrSignature` to a byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        let mut out = [0; 96];
        out[0..32].copy_from_slice(&self.sigma.to_bytes());
        out[32..64].copy_from_slice(&self.signature.to_bytes());
        out[64..96].copy_from_slice(&self.challenge.to_bytes());

        out
    }

    /// Convert a string of bytes into a `SchnorrSignature`.
    ///
    /// Not sure the sigma, s and c creation can fail if the 96 bytes are correctly extracted.
    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        if bytes.len() < 96 {
            return Err(anyhow!(SchnorrSignatureError::SerializationError))
                .with_context(|| "Not enough bytes provided.");
        }

        let sigma_bytes = bytes[0..32]
            .try_into()
            .map_err(|_| anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Failed to obtain sigma's bytes.")?;
        let sigma = JubjubExtended::from_bytes(&sigma_bytes)
            .into_option()
            .ok_or(anyhow!(SchnorrSignatureError::SerializationError))
            .with_context(|| "Unable to convert bytes into a sigma value.")?;

        let signature_bytes = bytes[64..96]
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
