use std::fmt::{Display, Formatter};
use std::num::TryFromIntError;
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::entities::arithmetic_operation_wrapper::{
    impl_add_to_wrapper, impl_partial_eq_to_wrapper, impl_sub_to_wrapper,
};
use crate::signable_builder::Beacon as SignableBeacon;

/// Epoch represents a Cardano epoch
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd, Ord,
)]
pub struct Epoch(pub u64);

impl Epoch {
    /// The epoch offset used for signers stake distribution and verification keys retrieval.
    pub const SIGNER_RETRIEVAL_OFFSET: i64 = -1;

    /// The epoch offset used to retrieve the signers stake distribution and verification keys that's
    /// currently being signed so it can be used in the next epoch.
    pub const NEXT_SIGNER_RETRIEVAL_OFFSET: u64 = 0;

    /// The epoch offset used for signers stake distribution and verification keys recording.
    pub const SIGNER_RECORDING_OFFSET: u64 = 1;

    /// The epoch offset used for aggregator protocol parameters recording.
    pub const PROTOCOL_PARAMETERS_RECORDING_OFFSET: u64 = 2;

    /// The epoch offset used to retrieve, given the epoch at which a signer registered, the epoch
    /// at which the signer can send single signatures.
    pub const SIGNER_SIGNING_OFFSET: u64 = 2;

    /// Computes a new Epoch by applying an epoch offset.
    ///
    /// Will fail if the computed epoch is negative.
    pub fn offset_by(&self, epoch_offset: i64) -> Result<Self, EpochError> {
        let epoch_new = self.0 as i64 + epoch_offset;
        if epoch_new < 0 {
            return Err(EpochError::EpochOffset(self.0, epoch_offset));
        }
        Ok(Epoch(epoch_new as u64))
    }

    /// Apply the [retrieval offset][Self::SIGNER_RETRIEVAL_OFFSET] to this epoch
    pub fn offset_to_signer_retrieval_epoch(&self) -> Result<Self, EpochError> {
        self.offset_by(Self::SIGNER_RETRIEVAL_OFFSET)
    }

    /// Apply the [next signer retrieval offset][Self::NEXT_SIGNER_RETRIEVAL_OFFSET] to this epoch
    pub fn offset_to_next_signer_retrieval_epoch(&self) -> Self {
        *self + Self::NEXT_SIGNER_RETRIEVAL_OFFSET
    }

    /// Apply the [recording offset][Self::SIGNER_RECORDING_OFFSET] to this epoch
    pub fn offset_to_recording_epoch(&self) -> Self {
        *self + Self::SIGNER_RECORDING_OFFSET
    }

    /// Apply the [protocol parameters recording offset][Self::PROTOCOL_PARAMETERS_RECORDING_OFFSET] to this epoch
    pub fn offset_to_protocol_parameters_recording_epoch(&self) -> Self {
        *self + Self::PROTOCOL_PARAMETERS_RECORDING_OFFSET
    }

    /// Apply the [signer signing offset][Self::SIGNER_SIGNING_OFFSET] to this epoch
    pub fn offset_to_signer_signing_offset(&self) -> Self {
        *self + Self::SIGNER_SIGNING_OFFSET
    }

    /// Computes the next Epoch
    pub fn next(&self) -> Self {
        *self + 1
    }

    /// Computes the previous Epoch
    pub fn previous(&self) -> Result<Self, EpochError> {
        self.offset_by(-1)
    }

    /// Check if there is a gap with another Epoch.
    pub fn has_gap_with(&self, other: &Epoch) -> bool {
        self.0.abs_diff(other.0) > 1
    }
}

impl Deref for Epoch {
    type Target = u64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Epoch {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl_add_to_wrapper!(Epoch, u64);
impl_sub_to_wrapper!(Epoch, u64);
impl_partial_eq_to_wrapper!(Epoch, u64);

impl Display for Epoch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl SignableBeacon for Epoch {}

impl TryInto<i64> for Epoch {
    type Error = TryFromIntError;

    fn try_into(self) -> Result<i64, Self::Error> {
        self.0.try_into()
    }
}

impl TryInto<i64> for &Epoch {
    type Error = TryFromIntError;

    fn try_into(self) -> Result<i64, Self::Error> {
        self.0.try_into()
    }
}

/// EpochError is an error triggerred by an [Epoch]
#[derive(Error, Debug)]
pub enum EpochError {
    /// Error raised when the [computation of an epoch using an offset][Epoch::offset_by] fails.
    #[error("epoch offset error")]
    EpochOffset(u64, i64),
}

#[cfg(test)]
mod tests {
    use crate::entities::arithmetic_operation_wrapper::tests::test_op_assign;

    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Epoch(72)), "72");
        assert_eq!(format!("{}", &Epoch(13224)), "13224");
    }

    #[test]
    fn test_serialize() {
        assert_eq!(serde_json::to_string(&Epoch(72)).unwrap(), "72");
    }

    #[test]
    fn test_deserialize() {
        let block_number: Epoch = serde_json::from_str("13224").unwrap();
        assert_eq!(block_number, Epoch(13224));
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_add() {
        assert_eq!(Epoch(4), Epoch(1) + Epoch(3));
        assert_eq!(Epoch(4), Epoch(1) + 3_u64);
        assert_eq!(Epoch(4), Epoch(1) + &3_u64);

        assert_eq!(Epoch(4), 3_u64 + Epoch(1));
        assert_eq!(Epoch(4), 3_u64 + &Epoch(1));
        assert_eq!(Epoch(4), &3_u64 + Epoch(1));
        assert_eq!(Epoch(4), &3_u64 + &Epoch(1));

        test_op_assign!(Epoch(1), +=, Epoch(3) => Epoch(4));
        test_op_assign!(Epoch(1), +=, 3_u64 => Epoch(4));
        test_op_assign!(Epoch(1), +=, &3_u64 => Epoch(4));

        test_op_assign!(1_u64, +=, Epoch(3) => 4_u64);
        test_op_assign!(1_u64, +=, &Epoch(3) => 4_u64);
    }

    #[test]
    #[allow(clippy::op_ref)]
    fn test_sub() {
        assert_eq!(Epoch(8), Epoch(14) - Epoch(6));
        assert_eq!(Epoch(8), Epoch(14) - 6_u64);
        assert_eq!(Epoch(8), Epoch(14) - &6_u64);

        assert_eq!(Epoch(8), 6_u64 - Epoch(14));
        assert_eq!(Epoch(8), 6_u64 - &Epoch(14));
        assert_eq!(Epoch(8), &6_u64 - Epoch(14));
        assert_eq!(Epoch(8), &6_u64 - &Epoch(14));

        test_op_assign!(Epoch(14), -=, Epoch(6) => Epoch(8));
        test_op_assign!(Epoch(14), -=, 6_u64 => Epoch(8));
        test_op_assign!(Epoch(14), -=, &6_u64 => Epoch(8));

        test_op_assign!(14_u64, -=, Epoch(6) => 8_u64);
        test_op_assign!(14_u64, -=, &Epoch(6) => 8_u64);
    }

    #[test]
    fn saturating_sub() {
        assert_eq!(Epoch(0), Epoch(1) - Epoch(5));
        assert_eq!(Epoch(0), Epoch(1) - 5_u64);
    }

    #[test]
    fn test_previous() {
        assert_eq!(Epoch(2), Epoch(3).previous().unwrap());
        assert!(Epoch(0).previous().is_err());
    }

    #[test]
    fn test_next() {
        assert_eq!(Epoch(4), Epoch(3).next());
    }

    #[test]
    fn test_eq() {
        assert_eq!(Epoch(1), Epoch(1));
        assert_eq!(Epoch(2), &Epoch(2));
        assert_eq!(&Epoch(3), Epoch(3));
        assert_eq!(&Epoch(4), &Epoch(4));

        assert_eq!(Epoch(5), 5);
        assert_eq!(Epoch(6), &6);
        assert_eq!(&Epoch(7), 7);
        assert_eq!(&Epoch(8), &8);

        assert_eq!(9, Epoch(9));
        assert_eq!(10, &Epoch(10));
        assert_eq!(&11, Epoch(11));
        assert_eq!(&12, &Epoch(12));
    }

    #[test]
    fn test_has_gap_ok() {
        assert!(Epoch(3).has_gap_with(&Epoch(5)));
        assert!(!Epoch(3).has_gap_with(&Epoch(4)));
        assert!(!Epoch(3).has_gap_with(&Epoch(3)));
        assert!(!Epoch(3).has_gap_with(&Epoch(2)));
        assert!(Epoch(3).has_gap_with(&Epoch(0)));
    }
}
