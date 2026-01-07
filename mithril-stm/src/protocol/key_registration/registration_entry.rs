use std::hash::Hash;

use serde::{Deserialize, Serialize};

use crate::{
    Initializer, RegisterError, Stake, StmResult,
    signature_scheme::{BlsVerificationKey, BlsVerificationKeyProofOfPossession},
};

#[cfg(feature = "future_snark")]
use crate::signature_scheme::SchnorrVerificationKey;

/// Wrapper of the Concatenation proof Verification key with proof of possession
pub type VerificationKeyProofOfPossessionForConcatenation = BlsVerificationKeyProofOfPossession;

/// Wrapper of the MultiSignature Verification key
pub type VerificationKeyForConcatenation = BlsVerificationKey;

/// Represents a signer registration entry
#[derive(PartialEq, Eq, Clone, PartialOrd, Ord, Debug, Copy, Serialize, Deserialize)]
pub struct RegistrationEntry(
    VerificationKeyForConcatenation,
    Stake,
    #[cfg(feature = "future_snark")]
    #[serde(skip)]
    Option<SchnorrVerificationKey>,
);

impl RegistrationEntry {
    pub fn new(
        bls_verification_key_proof_of_possession: VerificationKeyProofOfPossessionForConcatenation,
        #[cfg(feature = "future_snark")] schnorr_verification_key: Option<SchnorrVerificationKey>,
        stake: Stake,
    ) -> StmResult<Self> {
        bls_verification_key_proof_of_possession
            .verify_proof_of_possession()
            .map_err(|_| {
                RegisterError::KeyInvalid(Box::new(bls_verification_key_proof_of_possession.vk))
            })?;
        Ok(RegistrationEntry(
            bls_verification_key_proof_of_possession.vk,
            stake,
            #[cfg(feature = "future_snark")]
            schnorr_verification_key,
        ))
    }

    pub fn get_bls_verification_key(&self) -> VerificationKeyForConcatenation {
        self.0
    }

    pub fn get_stake(&self) -> Stake {
        self.1
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result.to_vec()
    }

    pub fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let bls_verification_key = VerificationKeyForConcatenation::from_bytes(bytes)?;
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(&bytes[96..]);
        let stake = Stake::from_be_bytes(u64_bytes);
        Ok(RegistrationEntry(
            bls_verification_key,
            stake,
            #[cfg(feature = "future_snark")]
            None,
        ))
    }
}

impl From<Initializer> for RegistrationEntry {
    fn from(initializer: Initializer) -> Self {
        Self(
            initializer.pk.vk,
            initializer.stake,
            #[cfg(feature = "future_snark")]
            None,
        )
    }
}

impl Hash for RegistrationEntry {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
        self.0.hash(state);
    }

    fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state)
        }
    }
}
