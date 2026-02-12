use serde::{Deserialize, Serialize, Serializer, ser::SerializeTuple};
use std::cmp::Ordering;
use std::hash::Hash;

use crate::{RegisterError, RegistrationEntry, Stake, StmResult, VerificationKeyForConcatenation};

#[cfg(feature = "future_snark")]
use crate::{LotteryTargetValue, VerificationKeyForSnark};

/// Represents a registration entry of a closed key registration.
#[derive(PartialEq, Eq, Clone, Debug, Copy, Deserialize)]
pub struct ClosedRegistrationEntry {
    verification_key_for_concatenation: VerificationKeyForConcatenation,
    stake: Stake,
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    verification_key_for_snark: Option<VerificationKeyForSnark>,
    #[cfg(feature = "future_snark")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    lottery_target_value: Option<LotteryTargetValue>,
}

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
        ClosedRegistrationEntry {
            verification_key_for_concatenation,
            stake,
            #[cfg(feature = "future_snark")]
            verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            lottery_target_value,
        }
    }

    /// Gets the verification key for concatenation.
    pub fn get_verification_key_for_concatenation(&self) -> VerificationKeyForConcatenation {
        self.verification_key_for_concatenation
    }

    /// Gets the stake.
    pub fn get_stake(&self) -> Stake {
        self.stake
    }

    #[cfg(feature = "future_snark")]
    /// Gets the verification key for snark.
    pub fn get_verification_key_for_snark(&self) -> Option<VerificationKeyForSnark> {
        self.verification_key_for_snark
    }

    #[cfg(feature = "future_snark")]
    /// Gets the lottery target value.
    pub fn get_lottery_target_value(&self) -> Option<LotteryTargetValue> {
        self.lottery_target_value
    }

    /// Converts the registration entry to bytes.
    /// Uses 96 bytes for the verification key for concatenation and 8 bytes for the stake
    /// (u64 big-endian).
    /// #[cfg(feature = "future_snark")] Uses 64 bytes for the verification key for snark and 32
    /// bytes for the lottery target value
    /// The order is backward compatible with previous implementations.
    pub(crate) fn to_bytes(self) -> Vec<u8> {
        #[cfg(feature = "future_snark")]
        let capacity = 200;
        #[cfg(not(feature = "future_snark"))]
        let capacity = 104;

        let mut result = Vec::with_capacity(capacity);
        result.extend_from_slice(&self.verification_key_for_concatenation.to_bytes());
        result.extend_from_slice(&self.stake.to_be_bytes());

        #[cfg(feature = "future_snark")]
        if let (Some(schnorr_vk), Some(target_value)) =
            (&self.verification_key_for_snark, &self.lottery_target_value)
        {
            result.extend_from_slice(&schnorr_vk.to_bytes());
            result.extend_from_slice(&target_value.to_bytes());
        }

        result
    }

    /// Creates a registration entry from bytes.
    /// Expects 96 bytes for the verification key for concatenation and 8 bytes for the stake
    /// (u64 big-endian).
    /// #[cfg(feature = "future_snark")] Expects 64 bytes for the verification key for snark and 32
    /// bytes for the lottery target value.
    /// The order is backward compatible with previous implementations.
    pub(crate) fn from_bytes(bytes: &[u8]) -> StmResult<Self> {
        let verification_key_for_concatenation = VerificationKeyForConcatenation::from_bytes(
            bytes.get(..96).ok_or(RegisterError::SerializationError)?,
        )?;
        let mut u64_bytes = [0u8; 8];
        u64_bytes.copy_from_slice(bytes.get(96..104).ok_or(RegisterError::SerializationError)?);
        let stake = Stake::from_be_bytes(u64_bytes);

        #[cfg(feature = "future_snark")]
        let (verification_key_for_snark, lottery_target_value) = {
            let schnorr_verification_key = bytes
                .get(104..168)
                .map(VerificationKeyForSnark::from_bytes)
                .transpose()?;

            let lottery_target_value =
                bytes.get(168..200).map(LotteryTargetValue::from_bytes).transpose()?;

            match (schnorr_verification_key, lottery_target_value) {
                (Some(_), None) | (None, Some(_)) => {
                    return Err(RegisterError::SerializationError.into());
                }
                _ => {}
            }
            (schnorr_verification_key, lottery_target_value)
        };

        Ok(ClosedRegistrationEntry {
            verification_key_for_concatenation,
            stake,
            #[cfg(feature = "future_snark")]
            verification_key_for_snark,
            #[cfg(feature = "future_snark")]
            lottery_target_value,
        })
    }
}

impl Serialize for ClosedRegistrationEntry {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        #[cfg(not(feature = "future_snark"))]
        {
            let mut tuple = serializer.serialize_tuple(2)?;
            tuple.serialize_element(&self.verification_key_for_concatenation)?;
            tuple.serialize_element(&self.stake)?;
            tuple.end()
        }
        #[cfg(feature = "future_snark")]
        {
            let has_snark_fields = self.verification_key_for_snark.is_some()
                && self.lottery_target_value.is_some()
                && cfg!(feature = "future_snark");
            let tuples_number = if has_snark_fields { 4 } else { 2 };
            let mut tuple = serializer.serialize_tuple(tuples_number)?;
            tuple.serialize_element(&self.verification_key_for_concatenation)?;
            tuple.serialize_element(&self.stake)?;
            if has_snark_fields {
                tuple.serialize_element(&self.verification_key_for_snark)?;
                tuple.serialize_element(&self.lottery_target_value)?;
            }
            tuple.end()
        }
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
        self.stake.hash(state);
        self.verification_key_for_concatenation.hash(state);
        #[cfg(feature = "future_snark")]
        {
            self.verification_key_for_snark.hash(state);
            self.lottery_target_value.hash(state);
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

impl Ord for ClosedRegistrationEntry {
    /// Orders by stake first, then by Verification key for concatenation.
    ///
    /// Note: this ordering intentionally excludes the snark fields
    /// (`VerificationKeyForSnark`, `LotteryTargetValue`), as we do not need them for ordering
    /// the Merkle tree leaves.
    fn cmp(&self, other: &Self) -> Ordering {
        self.stake.cmp(&other.stake).then(
            self.verification_key_for_concatenation
                .cmp(&other.verification_key_for_concatenation),
        )
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
            VerificationKeyForSnark::new_from_signing_key(sk.clone())
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

    mod golden {
        use super::*;

        #[cfg(not(feature = "future_snark"))]
        const GOLDEN_BYTES: &[u8; 104] = &[
            143, 161, 255, 48, 78, 57, 204, 220, 25, 221, 164, 252, 248, 14, 56, 126, 186, 135,
            228, 188, 145, 181, 52, 200, 97, 99, 213, 46, 0, 199, 193, 89, 187, 88, 29, 135, 173,
            244, 86, 36, 83, 54, 67, 164, 6, 137, 94, 72, 6, 105, 128, 128, 93, 48, 176, 11, 4,
            246, 138, 48, 180, 133, 90, 142, 192, 24, 193, 111, 142, 31, 76, 111, 110, 234, 153,
            90, 208, 192, 31, 124, 95, 102, 49, 158, 99, 52, 220, 165, 94, 251, 68, 69, 121, 16,
            224, 194, 0, 0, 0, 0, 0, 0, 0, 1,
        ];

        #[cfg(feature = "future_snark")]
        const GOLDEN_BYTES: &[u8; 200] = &[
            143, 161, 255, 48, 78, 57, 204, 220, 25, 221, 164, 252, 248, 14, 56, 126, 186, 135,
            228, 188, 145, 181, 52, 200, 97, 99, 213, 46, 0, 199, 193, 89, 187, 88, 29, 135, 173,
            244, 86, 36, 83, 54, 67, 164, 6, 137, 94, 72, 6, 105, 128, 128, 93, 48, 176, 11, 4,
            246, 138, 48, 180, 133, 90, 142, 192, 24, 193, 111, 142, 31, 76, 111, 110, 234, 153,
            90, 208, 192, 31, 124, 95, 102, 49, 158, 99, 52, 220, 165, 94, 251, 68, 69, 121, 16,
            224, 194, 0, 0, 0, 0, 0, 0, 0, 1, 200, 194, 6, 212, 77, 254, 23, 111, 33, 34, 139, 71,
            131, 196, 108, 13, 217, 75, 187, 131, 158, 77, 197, 163, 30, 123, 151, 237, 157, 232,
            167, 10, 45, 121, 194, 155, 110, 46, 240, 74, 141, 138, 78, 228, 92, 179, 58, 63, 233,
            239, 84, 114, 149, 77, 188, 93, 8, 22, 11, 12, 45, 186, 211, 56, 1, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ];

        fn golden_value() -> ClosedRegistrationEntry {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let bls_sk = BlsSigningKey::generate(&mut rng);
            let bls_pk = VerificationKeyProofOfPossessionForConcatenation::from(&bls_sk);

            #[cfg(feature = "future_snark")]
            let schnorr_verification_key = {
                let sk = SchnorrSigningKey::generate(&mut rng);
                VerificationKeyForSnark::new_from_signing_key(sk.clone())
            };
            ClosedRegistrationEntry::new(
                bls_pk.vk,
                1,
                #[cfg(feature = "future_snark")]
                Some(schnorr_verification_key),
                #[cfg(feature = "future_snark")]
                Some(LotteryTargetValue::get_one()),
            )
        }

        #[test]
        fn golden_conversions() {
            let value = ClosedRegistrationEntry::from_bytes(GOLDEN_BYTES)
                .expect("This from bytes should not fail");
            assert_eq!(golden_value(), value);

            let serialized = ClosedRegistrationEntry::to_bytes(value);
            let golden_serialized = ClosedRegistrationEntry::to_bytes(golden_value());
            assert_eq!(golden_serialized, serialized);
        }
    }
}
