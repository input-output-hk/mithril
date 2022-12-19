use crate::{
    NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET, SIGNER_EPOCH_RECORDING_OFFSET,
    SIGNER_EPOCH_RETRIEVAL_OFFSET,
};
use serde::{Deserialize, Serialize};
use std::{
    fmt::{Display, Formatter},
    ops::{Add, AddAssign, Sub, SubAssign},
};
use thiserror::Error;

/// Epoch represents a Cardano epoch
#[derive(
    Debug, Copy, Clone, Default, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd, Ord,
)]
pub struct Epoch(pub u64);

impl Epoch {
    /// Computes a new Epoch by applying an epoch offset.
    ///
    /// Will fails if the computed epoch is negative.
    pub fn offset_by(&self, epoch_offset: i64) -> Result<Self, EpochError> {
        let epoch_new = self.0 as i64 + epoch_offset;
        if epoch_new < 0 {
            return Err(EpochError::EpochOffset(self.0, epoch_offset));
        }
        Ok(Epoch(epoch_new as u64))
    }

    /// Apply the [SIGNER_EPOCH_RETRIEVAL_OFFSET] to this epoch
    pub fn offset_to_signer_retrieval_epoch(&self) -> Result<Self, EpochError> {
        self.offset_by(SIGNER_EPOCH_RETRIEVAL_OFFSET)
    }

    /// Apply the [NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET] to this epoch
    pub fn offset_to_next_signer_retrieval_epoch(&self) -> Self {
        *self + NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET
    }

    /// Apply the [SIGNER_EPOCH_RECORDING_OFFSET] to this epoch
    pub fn offset_to_recording_epoch(&self) -> Self {
        *self + SIGNER_EPOCH_RECORDING_OFFSET
    }
}

impl Add for Epoch {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Epoch(self.0 + rhs.0)
    }
}

impl Add<u64> for Epoch {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl Add<i32> for Epoch {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Self(self.0 + rhs as u64)
    }
}

impl Add<i64> for Epoch {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        Self(self.0 + rhs as u64)
    }
}

impl AddAssign for Epoch {
    fn add_assign(&mut self, rhs: Self) {
        *self = self.add(rhs);
    }
}

impl AddAssign<u64> for Epoch {
    fn add_assign(&mut self, rhs: u64) {
        *self = self.add(rhs);
    }
}

impl AddAssign<i32> for Epoch {
    fn add_assign(&mut self, rhs: i32) {
        *self = self.add(rhs);
    }
}

impl AddAssign<i64> for Epoch {
    fn add_assign(&mut self, rhs: i64) {
        *self = self.add(rhs);
    }
}

impl Sub for Epoch {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Sub<u64> for Epoch {
    type Output = Self;

    fn sub(self, rhs: u64) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl Sub<i32> for Epoch {
    type Output = Self;

    fn sub(self, rhs: i32) -> Self::Output {
        Self(self.0 - rhs as u64)
    }
}

impl Sub<i64> for Epoch {
    type Output = Self;

    fn sub(self, rhs: i64) -> Self::Output {
        Self(self.0 - rhs as u64)
    }
}

impl SubAssign for Epoch {
    fn sub_assign(&mut self, rhs: Self) {
        *self = self.sub(rhs);
    }
}

impl SubAssign<u64> for Epoch {
    fn sub_assign(&mut self, rhs: u64) {
        *self = self.sub(rhs);
    }
}

impl SubAssign<i32> for Epoch {
    fn sub_assign(&mut self, rhs: i32) {
        *self = self.sub(rhs);
    }
}

impl SubAssign<i64> for Epoch {
    fn sub_assign(&mut self, rhs: i64) {
        *self = self.sub(rhs);
    }
}

impl PartialEq<u64> for Epoch {
    fn eq(&self, other: &u64) -> bool {
        self.0.eq(other)
    }
}

impl PartialEq<Epoch> for u64 {
    fn eq(&self, other: &Epoch) -> bool {
        other.0.eq(self)
    }
}

impl Display for Epoch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(Epoch(4), Epoch(1) + Epoch(3));
        assert_eq!(Epoch(4), Epoch(1) + 3_u64);
        assert_eq!(Epoch(4), Epoch(1) + 3_i32);
        assert_eq!(Epoch(4), Epoch(1) + 3_i64);

        let mut epoch = Epoch(1);
        epoch += Epoch(3);
        assert_eq!(Epoch(4), epoch);

        let mut epoch = Epoch(1);
        epoch += 3_u64;
        assert_eq!(Epoch(4), epoch);

        let mut epoch = Epoch(1);
        epoch += 3_i32;
        assert_eq!(Epoch(4), epoch);

        let mut epoch = Epoch(1);
        epoch += 3_i64;
        assert_eq!(Epoch(4), epoch);
    }

    #[test]
    fn test_sub() {
        assert_eq!(Epoch(8), Epoch(14) - Epoch(6));
        assert_eq!(Epoch(8), Epoch(14) - 6_u64);
        assert_eq!(Epoch(8), Epoch(14) - 6_i32);
        assert_eq!(Epoch(8), Epoch(14) - 6_i64);

        let mut epoch = Epoch(14);
        epoch -= Epoch(6);
        assert_eq!(Epoch(8), epoch);

        let mut epoch = Epoch(14);
        epoch -= 6_u64;
        assert_eq!(Epoch(8), epoch);

        let mut epoch = Epoch(14);
        epoch -= 6_i32;
        assert_eq!(Epoch(8), epoch);

        let mut epoch = Epoch(14);
        epoch -= 6_i64;
        assert_eq!(Epoch(8), epoch);
    }
}
