use std::cmp::Ordering;
use std::hash::Hash;

use crate::{Stake, VerificationKeyForConcatenation};

#[cfg(feature = "future_snark")]
use crate::{LotteryTargetValue, VerificationKeyForSnark};

/// Represents a signer registration entry
#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub struct ClosedRegistrationEntry {
    verification_key_for_concatenation: VerificationKeyForConcatenation,
    stake: Stake,
    #[cfg(feature = "future_snark")]
    verification_key_for_snark: Option<VerificationKeyForSnark>,
    #[cfg(feature = "future_snark")]
    lottery_target_value: Option<LotteryTargetValue>,
}

impl ClosedRegistrationEntry {
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

    /// Gets the BLS verification key.
    pub fn get_bls_verification_key(&self) -> VerificationKeyForConcatenation {
        self.verification_key_for_concatenation
    }

    /// Gets the stake.
    pub fn get_stake(&self) -> Stake {
        self.stake
    }

    #[cfg(feature = "future_snark")]
    /// Gets the Schnorr verification key.
    pub fn get_schnorr_verification_key(&self) -> VerificationKeyForSnark {
        self.verification_key_for_snark.unwrap()
    }
    #[cfg(feature = "future_snark")]
    /// Gets the lottery target value.
    pub fn get_lottery_target_value(&self) -> LotteryTargetValue {
        self.lottery_target_value.unwrap()
    }
}

impl Hash for ClosedRegistrationEntry {
    /// Hashes the registration entry by hashing the stake first, then the verification key.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.stake.hash(state);
        self.verification_key_for_concatenation.hash(state);
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
    /// Compares the registration entries by comparing the stake first, then the verification key.
    fn cmp(&self, other: &Self) -> Ordering {
        self.stake.cmp(&other.stake).then(
            self.verification_key_for_concatenation
                .cmp(&other.verification_key_for_concatenation),
        )
    }
}
