use crate::{
    NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET, SIGNER_EPOCH_RECORDING_OFFSET,
    SIGNER_EPOCH_RETRIEVAL_OFFSET,
};
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::{Add, Sub};
use thiserror::Error;

/// Epoch represents a Cardano epoch
#[derive(Debug, Copy, Clone, Default, PartialEq, Serialize, Deserialize, Hash, Eq, PartialOrd)]
pub struct Epoch(pub u64);

impl Epoch {
    /// Computes a new Epoch by applying an epoch offset
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
    pub fn offset_to_next_signer_retrieval_epoch(&self) -> Result<Self, EpochError> {
        self.offset_by(NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET)
    }

    /// Apply the [SIGNER_EPOCH_RECORDING_OFFSET] to this epoch
    pub fn offset_to_recording_epoch(&self) -> Result<Self, EpochError> {
        self.offset_by(SIGNER_EPOCH_RECORDING_OFFSET)
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
        Epoch(self.0 + rhs)
    }
}

impl Sub<u64> for Epoch {
    type Output = Self;

    fn sub(self, rhs: u64) -> Self::Output {
        Epoch(self.0 - rhs)
    }
}

impl Add<i64> for Epoch {
    type Output = Self;

    fn add(self, rhs: i64) -> Self::Output {
        Epoch(self.0 + rhs as u64)
    }
}

impl Add<i32> for Epoch {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Epoch(self.0 + rhs as u64)
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
