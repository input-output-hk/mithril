//! Halo2-facing type aliases and witness shapes for the STM SNARK circuit.
//!
//! This module bridges STM domain concepts (message, lottery index, Merkle proof)
//! to circuit-oriented types consumed by the Halo2 relation and gadgets.
//! Wrapper types in this module represent the circuit statement layer.
//! Backend aliases are kept here to provide one shared source for engine-facing types.

use crate::circuits::halo2::backend_reexports::{BackendJubjub, BackendJubjubBase};
use crate::signature_scheme::SchnorrVerificationKey;
use ff::Field;
use std::ops::{Add, AddAssign, Neg, Sub};

/// Shared backend field alias used by Halo2 relation/chips.
pub(crate) type CircuitBase = BackendJubjubBase;
/// Shared backend curve alias used by Halo2 relation/chips.
pub(crate) type CircuitCurve = BackendJubjub;
/// Circuit-local wrapper for base-field values present in public inputs/witness data.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct CircuitBaseField(BackendJubjubBase);

impl CircuitBaseField {
    pub const ZERO: Self = Self(BackendJubjubBase::ZERO);
    pub const ONE: Self = Self(BackendJubjubBase::ONE);
}

impl From<u64> for CircuitBaseField {
    fn from(value: u64) -> Self {
        Self(BackendJubjubBase::from(value))
    }
}

impl From<BackendJubjubBase> for CircuitBaseField {
    fn from(value: BackendJubjubBase) -> Self {
        Self(value)
    }
}

impl From<CircuitBaseField> for BackendJubjubBase {
    fn from(value: CircuitBaseField) -> Self {
        value.0
    }
}

impl Add for CircuitBaseField {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for CircuitBaseField {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl Sub for CircuitBaseField {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl Neg for CircuitBaseField {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self(-self.0)
    }
}

/// Lottery threshold value used by the circuit for signer eligibility checks.
pub type Target = CircuitBaseField;
/// Signed message value used by the circuit transcript, without any domain prefix.
pub type SignedMessageWithoutPrefix = CircuitBaseField;
/// Merkle root public input committed by the STM membership commitment tree.
pub type MerkleRoot = CircuitBaseField;
/// Lottery index (`i`) used for per-lottery checks in witness entries.
pub type LotteryIndex = u32;

/// Merkle-tree leaf material used by Halo2 witness construction.
///
/// The first field stores the signer's verification key, and the second
/// field stores the lottery target value associated with that signer.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MTLeaf(pub SchnorrVerificationKey, pub Target);

/// Position of a sibling node relative to the current hash in a Merkle path.
#[derive(Clone, Copy, Debug)]
pub enum Position {
    Left,
    Right,
}

/// Merkle authentication path used by the Halo2 circuit witness.
///
/// Each entry stores sibling position and sibling hash value for one tree level.
#[derive(Clone, Debug)]
pub struct MerklePath {
    /// Ordered list of `(position, sibling_hash)` from leaf level to root level.
    pub siblings: Vec<(Position, CircuitBaseField)>,
}

impl MerklePath {
    /// Creates a new Merkle path from ordered sibling entries.
    pub fn new(siblings: Vec<(Position, CircuitBaseField)>) -> Self {
        Self { siblings }
    }
}
