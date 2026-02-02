use std::cmp::Ordering;
use std::hash::Hash;

use serde::{Deserialize, Serialize};

#[cfg(feature = "future_snark")]
use crate::VerificationKeyForSnark;
use crate::{
    Initializer, RegisterError, Stake, StmResult, VerificationKeyForConcatenation,
    VerificationKeyProofOfPossessionForConcatenation,
};

use super::ClosedRegistrationEntry;

/// Represents a signer registration entry
#[derive(PartialEq, Eq, Clone, Debug, Copy, Serialize, Deserialize)]
pub struct RegistrationEntry(
    VerificationKeyForConcatenation,
    Stake,
    #[cfg(feature = "future_snark")]
    #[serde(skip)]
    Option<VerificationKeyForSnark>,
);

impl RegistrationEntry {
    /// Creates a new registration entry. Verifies the proof of possession before creating the
    /// entry. Fails if the proof of possession is invalid.
    pub fn new(
        bls_verification_key_proof_of_possession: VerificationKeyProofOfPossessionForConcatenation,
        stake: Stake,
        #[cfg(feature = "future_snark")] schnorr_verification_key: Option<VerificationKeyForSnark>,
    ) -> StmResult<Self> {
        bls_verification_key_proof_of_possession
            .verify_proof_of_possession()
            .map_err(|_| {
                RegisterError::ConcatenationKeyInvalid(Box::new(
                    bls_verification_key_proof_of_possession.vk,
                ))
            })?;

        #[cfg(feature = "future_snark")]
        if let Some(schnorr_vk) = schnorr_verification_key {
            schnorr_vk
                .is_valid()
                .map_err(|_| RegisterError::SnarkKeyInvalid(Box::new(schnorr_vk)))?;
        }

        Ok(RegistrationEntry(
            bls_verification_key_proof_of_possession.vk,
            stake,
            #[cfg(feature = "future_snark")]
            schnorr_verification_key,
        ))
    }

    /// Gets the BLS verification key.
    pub fn get_bls_verification_key(&self) -> VerificationKeyForConcatenation {
        self.0
    }

    #[cfg(feature = "future_snark")]
    /// Gets the BLS verification key.
    pub fn get_schnorr_verification_key(&self) -> VerificationKeyForSnark {
        self.2.unwrap()
    }

    /// Gets the stake associated with the registration entry.
    pub fn get_stake(&self) -> Stake {
        self.1
    }

    pub fn to_closed_registration_entry(&self, _total_stake: Stake) -> ClosedRegistrationEntry {
        ClosedRegistrationEntry::new(
            self.0,
            self.1,
            #[cfg(feature = "future_snark")]
            self.2,
            #[cfg(feature = "future_snark")]
            None,
        )
    }

    /// Converts the registration entry to bytes.
    /// Uses 96 bytes for the BLS verification key and 8 bytes for the stake (u64 big-endian).
    /// The order is backward compatible with previous implementations.
    pub(crate) fn to_bytes(self) -> Vec<u8> {
        let mut result = [0u8; 104];
        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..].copy_from_slice(&self.1.to_be_bytes());
        result.to_vec()
    }

    /// Creates a registration entry from bytes.
    /// Expects 96 bytes for the BLS verification key and 8 bytes for the stake (u64 big-endian).
    /// The order is backward compatible with previous implementations.
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
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

impl From<ClosedRegistrationEntry> for RegistrationEntry {
    fn from(entry: ClosedRegistrationEntry) -> Self {
        RegistrationEntry(
            entry.get_bls_verification_key(),
            entry.get_stake(),
            #[cfg(feature = "future_snark")]
            Some(entry.get_schnorr_verification_key()),
        )
    }
}

impl From<Initializer> for RegistrationEntry {
    fn from(initializer: Initializer) -> Self {
        Self(
            initializer.bls_verification_key_proof_of_possession.vk,
            initializer.stake,
            #[cfg(feature = "future_snark")]
            initializer.schnorr_verification_key,
        )
    }
}

impl Hash for RegistrationEntry {
    /// Hashes the registration entry by hashing the stake first, then the verification key.
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

impl PartialOrd for RegistrationEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

impl Ord for RegistrationEntry {
    /// Compares the registration entries by comparing the stake first, then the verification key.
    fn cmp(&self, other: &Self) -> Ordering {
        self.1.cmp(&other.1).then(self.0.cmp(&other.0))
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;
    use std::cmp::Ordering;

    use crate::{
        VerificationKeyProofOfPossessionForConcatenation, signature_scheme::BlsSigningKey,
    };

    #[cfg(feature = "future_snark")]
    use crate::{VerificationKeyForSnark, signature_scheme::SchnorrSigningKey};

    use super::*;

    fn create_registration_entry(rng: &mut ChaCha20Rng, stake: Stake) -> RegistrationEntry {
        let bls_sk = BlsSigningKey::generate(rng);
        let bls_pk = VerificationKeyProofOfPossessionForConcatenation::from(&bls_sk);

        #[cfg(feature = "future_snark")]
        let schnorr_verification_key = {
            let sk = SchnorrSigningKey::generate(rng).unwrap();
            VerificationKeyForSnark::new_from_signing_key(sk.clone()).unwrap()
        };
        RegistrationEntry::new(
            bls_pk,
            stake,
            #[cfg(feature = "future_snark")]
            Some(schnorr_verification_key),
        )
        .unwrap()
    }

    #[test]
    fn test_ord_different_stakes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

        let entry_low_stake = create_registration_entry(&mut rng, 100);
        let entry_high_stake = create_registration_entry(&mut rng, 200);

        assert_eq!(entry_low_stake.cmp(&entry_high_stake), Ordering::Less);
        assert_eq!(entry_high_stake.cmp(&entry_low_stake), Ordering::Greater);
    }

    #[test]
    fn test_ord_same_stake_different_keys() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

        let entry1 = create_registration_entry(&mut rng, 100);
        let entry2 = create_registration_entry(&mut rng, 100);

        let cmp_result = entry1.cmp(&entry2);
        assert!(cmp_result == Ordering::Less || cmp_result == Ordering::Greater);

        assert_eq!(entry2.cmp(&entry1), cmp_result.reverse());
    }
}
