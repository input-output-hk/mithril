use std::{
    cmp::Ordering,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    iter::Sum,
};

use blst::{
    BLST_ERROR,
    min_sig::{AggregatePublicKey, PublicKey as BlstVk},
};
use serde::{Deserialize, Serialize};

use crate::bls_multi_signature::{
    BlsProofOfPossession, BlsSigningKey, POP, helper::unsafe_helpers::verify_pairing,
};
use crate::error::{MultiSignatureError, blst_err_to_mithril};

/// MultiSig verification key, which is a wrapper over the BlstVk (element in G2)
/// from the blst library.
#[derive(Debug, Clone, Copy, Default)]
pub struct BlsVerificationKey(pub BlstVk);

impl BlsVerificationKey {
    /// Convert an `VerificationKey` to its compressed byte representation.
    pub fn to_bytes(self) -> [u8; 96] {
        self.0.to_bytes()
    }

    /// Convert a compressed byte string into a `VerificationKey`.
    ///
    /// # Error
    /// This function fails if the bytes do not represent a compressed point of the prime
    /// order subgroup of the curve Bls12-381.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let bytes = bytes.get(..96).ok_or(MultiSignatureError::SerializationError)?;
        match BlstVk::key_validate(bytes) {
            Ok(vk) => Ok(Self(vk)),
            Err(e) => Err(blst_err_to_mithril(e, None, None)
                .expect_err("If deserialization is not successful, blst returns and error different to SUCCESS."))
        }
    }

    /// Compare two `VerificationKey`. Used for PartialOrd impl, used to order signatures. The comparison
    /// function can be anything, as long as it is consistent.
    fn compare_verification_keys(&self, other: &BlsVerificationKey) -> Ordering {
        let self_bytes = self.to_bytes();
        let other_bytes = other.to_bytes();
        let mut result = Ordering::Equal;

        for (i, j) in self_bytes.iter().zip(other_bytes.iter()) {
            result = i.cmp(j);
            if result != Ordering::Equal {
                return result;
            }
        }

        result
    }

    pub(crate) fn to_blst_verification_key(self) -> BlstVk {
        self.0
    }
}

impl Display for BlsVerificationKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.to_bytes())
    }
}

impl Hash for BlsVerificationKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash_slice(&self.to_bytes(), state)
    }
}

impl PartialEq for BlsVerificationKey {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for BlsVerificationKey {}

impl PartialOrd for BlsVerificationKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for BlsVerificationKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare_verification_keys(other)
    }
}

impl<'a> Sum<&'a Self> for BlsVerificationKey {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        let keys: Vec<&BlstVk> = iter.map(|x| &x.0).collect();

        assert!(!keys.is_empty(), "One cannot add an empty vector");
        let aggregate_key = AggregatePublicKey::aggregate(&keys, false)
            .expect("An MspMvk is always a valid key. This function only fails if keys is empty or if the keys are invalid, none of which can happen.")
            .to_public_key();

        Self(aggregate_key)
    }
}

impl From<&BlsSigningKey> for BlsVerificationKey {
    /// Convert a secret key into an `MspMvk`. This is performed by computing
    /// `MspMvk = g2 * sk`, where `g2` is the generator in G2. We can use the
    /// blst built-in function `sk_to_pk`.
    fn from(sk: &BlsSigningKey) -> Self {
        BlsVerificationKey(sk.to_blst_secret_key().sk_to_pk())
    }
}

/// MultiSig public key, contains the verification key and the proof of possession.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlsVerificationKeyProofOfPossession {
    /// The verification key.
    pub vk: BlsVerificationKey,
    /// Proof of Possession.
    pub pop: BlsProofOfPossession,
}

impl BlsVerificationKeyProofOfPossession {
    /// if `e(k1,g2) = e(H_G1("PoP" || mvk),mvk)` and `e(g1,mvk) = e(k2,g2)`
    /// are both true, return 1. The first part is a signature verification
    /// of message "PoP", while the second we need to compute the pairing
    /// manually.
    // If we are really looking for performance improvements, we can combine the
    // two final exponentiations (for verifying k1 and k2) into a single one.
    pub(crate) fn verify_proof_of_possession(&self) -> Result<(), MultiSignatureError> {
        match self.vk.to_blst_verification_key().validate() {
            Ok(_) => {
                let result = verify_pairing(&self.vk, &self.pop);
                if !(self.pop.get_k1().verify(
                    false,
                    POP,
                    &[],
                    &[],
                    &self.vk.to_blst_verification_key(),
                    false,
                ) == BLST_ERROR::BLST_SUCCESS
                    && result)
                {
                    return Err(MultiSignatureError::KeyInvalid(Box::new(*self)));
                }
                Ok(())
            }
            Err(e) => blst_err_to_mithril(e, None, Some(self.vk)),
        }
    }

    /// if `e(k1,g2) = e(H_G1("PoP" || mvk),mvk)` and `e(g1,mvk) = e(k2,g2)`
    /// are both true, return 1. The first part is a signature verification
    /// of message "PoP", while the second we need to compute the pairing
    /// manually.
    // If we are really looking for performance improvements, we can combine the
    // two final exponentiations (for verifying k1 and k2) into a single one.
    #[deprecated(since = "0.5.0", note = "Use `verify_proof_of_possession` instead")]
    pub fn check(&self) -> Result<(), MultiSignatureError> {
        Self::verify_proof_of_possession(self)
    }

    /// Convert to a 144 byte string.
    ///
    /// # Layout
    /// The layout of a `PublicKeyPoP` encoding is
    /// * Public key
    /// * Proof of Possession
    pub fn to_bytes(self) -> [u8; 192] {
        let mut vkpop_bytes = [0u8; 192];
        vkpop_bytes[..96].copy_from_slice(&self.vk.to_bytes());
        vkpop_bytes[96..].copy_from_slice(&self.pop.to_bytes());
        vkpop_bytes
    }

    /// Deserialize a byte string to a `BlsVerificationKeyProofOfPossession`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, MultiSignatureError> {
        let mvk = BlsVerificationKey::from_bytes(
            bytes.get(..96).ok_or(MultiSignatureError::SerializationError)?,
        )?;

        let pop = BlsProofOfPossession::from_bytes(
            bytes.get(96..).ok_or(MultiSignatureError::SerializationError)?,
        )?;

        Ok(Self { vk: mvk, pop })
    }
}

impl From<&BlsSigningKey> for BlsVerificationKeyProofOfPossession {
    /// Convert a secret key into a `BlsVerificationKeyProofOfPossession` by simply converting to a
    /// `MspMvk` and `MspPoP`.
    fn from(sk: &BlsSigningKey) -> Self {
        Self {
            vk: sk.into(),
            pop: sk.into(),
        }
    }
}
