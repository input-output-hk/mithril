use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::hash::Hash;

use crate::{RegisterError, RegistrationEntry, Stake, StmResult, VerificationKeyForConcatenation};

#[cfg(feature = "future_snark")]
use crate::{LotteryTargetValue, VerificationKeyForSnark};

/// Represents a registration entry of a closed key registration.
#[derive(PartialEq, Eq, Clone, Debug, Copy, Serialize, Deserialize)]
pub struct ClosedRegistrationEntry(
    VerificationKeyForConcatenation,
    Stake,
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none")]
    Option<VerificationKeyForSnark>,
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none")]
    Option<LotteryTargetValue>,
);

impl ClosedRegistrationEntry {
    /// Creates a new closed registration entry.
    pub fn new(
        verification_key_for_concatenation: VerificationKeyForConcatenation,
        stake: Stake,
        #[cfg(feature = "future_snark")] verification_key_for_snark: Option<
            VerificationKeyForSnark,
        >,
        #[cfg(feature = "future_snark")] lottery_target_value: Option<LotteryTargetValue>,
    ) -> Self {
        ClosedRegistrationEntry(
            verification_key_for_concatenation,
            stake,
            #[cfg(feature = "future_snark")]
            verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            lottery_target_value,
        )
    }

    /// Gets the verification key for concatenation.
    pub fn get_verification_key_for_concatenation(&self) -> VerificationKeyForConcatenation {
        self.0
    }

    /// Gets the stake.
    pub fn get_stake(&self) -> Stake {
        self.1
    }

    #[cfg(feature = "future_snark")]
    /// Gets the verification key for snark.
    pub fn get_verification_key_for_snark(&self) -> Option<VerificationKeyForSnark> {
        self.2
    }

    #[cfg(feature = "future_snark")]
    /// Gets the lottery target value.
    pub fn get_lottery_target_value(&self) -> Option<LotteryTargetValue> {
        self.3
    }

    /// Converts the registration entry to bytes.
    /// Uses 96 bytes for the verification key for concatenation and 8 bytes for the stake
    /// (u64 big-endian).
    /// #[cfg(feature = "future_snark")] Uses 64 bytes for the verification key for snark and 32
    /// bytes for the lottery target value
    /// The order is backward compatible with previous implementations.
    pub(crate) fn to_bytes(self) -> Vec<u8> {
        #[cfg(feature = "future_snark")]
        let mut result = [0u8; 200];
        #[cfg(not(feature = "future_snark"))]
        let mut result = [0u8; 104];

        result[..96].copy_from_slice(&self.0.to_bytes());
        result[96..104].copy_from_slice(&self.1.to_be_bytes());

        #[cfg(feature = "future_snark")]
        {
            if let Some(schnorr_vk) = &self.2 {
                result[104..168].copy_from_slice(&schnorr_vk.to_bytes());
            }
            if let Some(target_value) = &self.3 {
                result[168..200].copy_from_slice(&target_value.to_bytes());
            }
        }

        result.to_vec()
    }

    /// Creates a registration entry from bytes.
    /// Expects 96 bytes for the verification key for concatenation and 8 bytes for the stake
    /// (u64 big-endian).
    /// #[cfg(feature = "future_snark")] Expects 64 bytes for the verification key for snark and 32
    /// bytes for the lottery target value.
    /// The order is backward compatible with previous implementations.
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let bls_verification_key = VerificationKeyForConcatenation::from_bytes(
            bytes.get(..96).ok_or(RegisterError::SerializationError)?,
        )?;
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(96..104).ok_or(RegisterError::SerializationError)?);
        let stake = Stake::from_be_bytes(u64_bytes);

        #[cfg(feature = "future_snark")]
        let (schnorr_verification_key, lottery_target_value) = {
            let vk = VerificationKeyForSnark::from_bytes(
                bytes.get(104..168).ok_or(RegisterError::SerializationError)?,
            )?;
            let target = LotteryTargetValue::from_bytes(
                bytes.get(168..200).ok_or(RegisterError::SerializationError)?,
            )?;
            (Some(vk), Some(target))
        };

        Ok(ClosedRegistrationEntry(
            bls_verification_key,
            stake,
            #[cfg(feature = "future_snark")]
            schnorr_verification_key,
            #[cfg(feature = "future_snark")]
            lottery_target_value,
        ))
    }
}

/// Converts the registration entry into a closed registration entry for given total stake.
/// This is where we will compute the lottery target value in the future.
/// `LotteryTargetValue` is set to one for now.
/// TODO: Compute the lottery target value based on the total stake and the entry's stake.
impl From<(RegistrationEntry, Stake)> for ClosedRegistrationEntry {
    fn from(entry_total_stake: (RegistrationEntry, Stake)) -> Self {
        let (entry, _total_stake) = entry_total_stake;
        #[cfg(feature = "future_snark")]
        let (schnorr_verification_key, target_value) = {
            let vk = entry.get_verification_key_for_snark();
            let target = vk.map(|_| LotteryTargetValue::get_one());
            (vk, target)
        };

        ClosedRegistrationEntry::new(
            entry.get_verification_key_for_concatenation(),
            entry.get_stake(),
            #[cfg(feature = "future_snark")]
            schnorr_verification_key,
            #[cfg(feature = "future_snark")]
            target_value,
        )
    }
}

impl Hash for ClosedRegistrationEntry {
    /// Hashes the registration entry by hashing the stake first, then the verification key.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.1.hash(state);
        self.0.hash(state);
        #[cfg(feature = "future_snark")]
        {
            self.2.hash(state);
            self.3.hash(state);
        }
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

impl PartialOrd for ClosedRegistrationEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}

// TODO: Update when `future_snark` is activated
impl Ord for ClosedRegistrationEntry {
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

    fn create_closed_registration_entry(
        rng: &mut ChaCha20Rng,
        stake: Stake,
    ) -> ClosedRegistrationEntry {
        let bls_sk = BlsSigningKey::generate(rng);
        let bls_pk = VerificationKeyProofOfPossessionForConcatenation::from(&bls_sk);

        #[cfg(feature = "future_snark")]
        let schnorr_verification_key = {
            let sk = SchnorrSigningKey::generate(rng);
            VerificationKeyForSnark::new_from_signing_key(sk.clone()).unwrap()
        };
        ClosedRegistrationEntry::new(
            bls_pk.vk,
            stake,
            #[cfg(feature = "future_snark")]
            Some(schnorr_verification_key),
            #[cfg(feature = "future_snark")]
            Some(LotteryTargetValue::get_one()),
        )
    }

    #[test]
    fn test_ord_different_stakes() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

        let entry_low_stake = create_closed_registration_entry(&mut rng, 100);
        let entry_high_stake = create_closed_registration_entry(&mut rng, 200);

        assert_eq!(entry_low_stake.cmp(&entry_high_stake), Ordering::Less);
        assert_eq!(entry_high_stake.cmp(&entry_low_stake), Ordering::Greater);
    }

    #[test]
    fn test_ord_same_stake_different_keys() {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

        let entry1 = create_closed_registration_entry(&mut rng, 100);
        let entry2 = create_closed_registration_entry(&mut rng, 100);

        let cmp_result = entry1.cmp(&entry2);
        assert!(cmp_result == Ordering::Less || cmp_result == Ordering::Greater);

        assert_eq!(entry2.cmp(&entry1), cmp_result.reverse());
    }
}
