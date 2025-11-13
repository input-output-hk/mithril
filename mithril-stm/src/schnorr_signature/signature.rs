use anyhow::{Result, anyhow};
use midnight_circuits::hash::poseidon::PoseidonChip;
use midnight_circuits::instructions::HashToCurveCPU;
use midnight_circuits::instructions::hash::HashCPU;
use midnight_curves::{Fq as JubjubBase, Fr as JubjubScalar, JubjubSubgroup};

use group::{Group, GroupEncoding};

use crate::{
    Index,
    schnorr_signature::{
        DST_LOTTERY, DST_SIGNATURE, JubjubHashToCurve, SchnorrVerificationKey, get_coordinates,
        hash_msg_to_jubjubbase, jubjub_base_to_scalar,
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
    pub(crate) sigma: JubjubSubgroup,
    /// Part of the Schnorr signature depending on the secret key
    pub(crate) signature: JubjubScalar,
    /// Part of the Schnorr signature NOT depending on the secret key
    pub(crate) challenge: JubjubBase,
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
    pub fn verify(&self, msg: &[u8], vk: &SchnorrVerificationKey) -> Result<()> {
        let generator = JubjubSubgroup::generator();

        // First hashing the message to a scalar then hashing it to a curve point
        let hash_msg = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_jubjubbase(msg)?]);

        // Computing R1 = H(msg) * s + sigma * c
        let challenge_scalar = jubjub_base_to_scalar(&self.challenge)?;
        let hash_msg_times_sig = hash_msg * self.signature;
        let sigma_times_challenge = self.sigma * challenge_scalar;
        let random_value_1_recomputed = hash_msg_times_sig + sigma_times_challenge;

        // Computing R2 = g * s + vk * c
        let generator_times_s = generator * self.signature;
        let vk_times_challenge = vk.0 * challenge_scalar;
        let random_value_2_recomputed = generator_times_s + vk_times_challenge;

        let (hashx, hashy) = get_coordinates(hash_msg);
        let (vkx, vky) = get_coordinates(vk.0);
        let (sigmax, sigmay) = get_coordinates(self.sigma);
        let (r1x, r1y) = get_coordinates(random_value_1_recomputed);
        let (r2x, r2y) = get_coordinates(random_value_2_recomputed);

        let challenge_recomputed = PoseidonChip::<JubjubBase>::hash(&[
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

        if challenge_recomputed != self.challenge {
            // TODO: Wrong error for now, need to change that once the errors are added
            return Err(anyhow!("Signature failed to verify."));
        }

        Ok(())
    }

    /// Dense mapping function indexed by the index to be evaluated adapted to the Schnorr signature.
    ///
    /// We need to convert the inputs to fit in a Poseidon hash.
    /// The order of the hash input must be the same as the one in the SNARK circuit
    /// `ev = H(DST || msg || index || σ) <- MSP.Eval(msg,index,σ)` given in paper.
    pub(crate) fn evaluate_dense_mapping(&self, msg: &[u8], index: Index) -> Result<[u8; 32]> {
        let hash = JubjubHashToCurve::hash_to_curve(&[hash_msg_to_jubjubbase(msg)?]);
        let (hashx, hashy) = get_coordinates(hash);
        // TODO: Check if this is the correct way to add the index
        let idx = JubjubBase::from_raw([0, 0, 0, index]);
        let (sigmax, sigmay) = get_coordinates(self.sigma);
        let ev =
            PoseidonChip::<JubjubBase>::hash(&[DST_LOTTERY, hashx, hashy, idx, sigmax, sigmay]);

        Ok(ev.to_bytes_le())
    }

    /// Convert an `SchnorrSignature` to a byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        let mut out = [0; 96];
        out[0..32].copy_from_slice(&self.sigma.to_bytes());
        out[32..64].copy_from_slice(&self.signature.to_bytes());
        out[64..96].copy_from_slice(&self.challenge.to_bytes_le());

        out
    }

    /// Convert a string of bytes into a `SchnorrSignature`.
    ///
    /// Not sure the sigma, s and c creation can fail if the 96 bytes are correctly extracted.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self> {
        let bytes: [u8; 96] = bytes.try_into()?;
        let sigma = JubjubSubgroup::from_bytes(&bytes[0..32].try_into()?)
            .into_option()
            .ok_or(anyhow!("Unable to convert bytes into a sigma value."))?;
        let signature = JubjubScalar::from_bytes(&bytes[32..64].try_into()?)
            .into_option()
            .ok_or(anyhow!("Unable to convert bytes into an s value."))?;
        let challenge = JubjubBase::from_bytes_le(&bytes[64..96].try_into()?)
            .into_option()
            .ok_or(anyhow!("Unable to convert bytes into a c value."))?;

        Ok(Self {
            sigma,
            signature,
            challenge,
        })
    }
}
