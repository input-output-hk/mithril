use anyhow::{Result, anyhow};

use dusk_jubjub::{
    ExtendedPoint as JubjubExtended, Fr as JubjubScalar, SubgroupPoint as JubjubSubgroup,
};
use dusk_poseidon::{Domain, Hash};
use group::{Group, GroupEncoding};

use crate::schnorr_signature::{
    DST_SIGNATURE, SchnorrVerificationKey, get_coordinates_extended, get_coordinates_subgroup,
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
    /// Description of the verification for Schnorr
    ///
    /// This function performs the verification of a Schnorr signature given the signature, the signed message
    /// and a verification key derived from the secret key used to sign.
    ///
    /// Input:
    ///     - a Schnorr signature
    ///     - a message: some bytes
    ///     - a verification key: a value depending on the secret key
    ///
    /// Output:
    ///     - Ok(()) if the signature verifies and an error if not
    ///
    /// Description of the verification algorithm:
    ///
    /// The verification algorithm follows the verification protocol of the Schnorr signature. It consists in
    /// recomputing the challenge value based on the value of the signature and checking the equality between
    /// the recomputed challenge and the one present in the Schnorr Signature.
    ///
    /// The step are the following:
    ///     - Compute the Hash of the message: H(Sha256(msg))
    ///     - Recompute the value R1 as: r1_tilde = h * s + sigma * c
    ///     - Recompute the value R2 as: r2_tilde = g * s + vk * c
    ///     - Use the recomputed values to recompute the Poseidon hash:
    /// c_tilde = Poseidon(DST || H(Sha256(msg)) || vk || sigma || R1 || R2), where each elliptic curve  point is converted
    /// to their coordinates representation to feed them to the hash function.
    ///     - Check: c == c_tilde
    ///     
    pub fn verify(&self, msg: &[u8], verification_key: &SchnorrVerificationKey) -> Result<()> {
        let generator = JubjubSubgroup::generator();

        // First hashing the message to a scalar then hashing it to a curve point
        let hash_msg = JubjubExtended::hash_to_point(msg);

        // Computing R1 = H(msg) * s + sigma * c
        let hash_msg_times_sig = hash_msg * self.signature;
        let sigma_times_challenge = self.sigma * self.challenge;
        let random_value_1_recomputed = hash_msg_times_sig + sigma_times_challenge;

        // Computing R2 = g * s + vk * c
        let generator_times_s = generator * self.signature;
        let vk_times_challenge = verification_key.0 * self.challenge;
        let random_value_2_recomputed = generator_times_s + vk_times_challenge;

        let (hash_msg_x, hash_msg_y) = get_coordinates_extended(hash_msg);
        let (verification_key_x, verification_key_y) = get_coordinates_subgroup(verification_key.0);
        let (sigma_x, sigma_y) = get_coordinates_extended(self.sigma);
        let (random_value_1_recomputed_x, random_value_1_recomputed_y) =
            get_coordinates_extended(random_value_1_recomputed);
        let (random_value_2_recomputed_x, random_value_2_recomputed_y) =
            get_coordinates_subgroup(random_value_2_recomputed);
        let challenge_recomputed = Hash::digest_truncated(
            Domain::Other,
            &[
                DST_SIGNATURE,
                hash_msg_x,
                hash_msg_y,
                verification_key_x,
                verification_key_y,
                sigma_x,
                sigma_y,
                random_value_1_recomputed_x,
                random_value_1_recomputed_y,
                random_value_2_recomputed_x,
                random_value_2_recomputed_y,
            ],
        )[0];

        if challenge_recomputed != self.challenge {
            // TODO: Wrong error for now, need to change that once the errors are added
            return Err(anyhow!("Signature failed to verify."));
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
    pub fn from_bytes(bytes: &[u8]) -> Result<Self> {
        let bytes: [u8; 96] = bytes.try_into()?;
        let sigma = JubjubExtended::from_bytes(&bytes[0..32].try_into()?)
            .into_option()
            .ok_or(anyhow!("Unable to convert bytes into a sigma value."))?;
        let signature = JubjubScalar::from_bytes(&bytes[32..64].try_into()?)
            .into_option()
            .ok_or(anyhow!("Unable to convert bytes into an s value."))?;
        let challenge = JubjubScalar::from_bytes(&bytes[64..96].try_into()?)
            .into_option()
            .ok_or(anyhow!("Unable to convert bytes into a c value."))?;

        Ok(Self {
            sigma,
            signature,
            challenge,
        })
    }
}
