//! Halo2-facing type aliases and witness shapes for the STM SNARK circuit.
//!
//! This module bridges STM domain concepts (message, lottery index, Merkle proof)
//! to circuit-oriented types consumed by the Halo2 relation and gadgets.
//! Wrapper types in this module represent the circuit statement layer.

use std::ops::{Add, AddAssign, Neg, Sub};

use ff::Field;
use midnight_curves::{Fq as MidnightBaseField, JubjubExtended as MidnightJubjub};

use crate::signature_scheme::{BaseFieldElement, SchnorrVerificationKey};

/// Shared Midnight field alias used by Halo2 relation/chips.
pub(crate) type CircuitBase = MidnightBaseField;
/// Shared Midnight curve alias used by Halo2 relation/chips.
pub(crate) type CircuitCurve = MidnightJubjub;

/// Field type boundaries:
/// - `BaseFieldElement`: STM/domain field wrapper.
/// - `CircuitBaseField`: circuit statement/input wrapper.
/// - `CircuitBase`: raw proving-library field.
///
/// Circuit-local wrapper for base-field values present in public inputs/witness data.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct CircuitBaseField(CircuitBase);

impl CircuitBaseField {
    /// Additive identity in the circuit base field.
    pub const ZERO: Self = Self(CircuitBase::ZERO);
    /// Multiplicative identity in the circuit base field.
    pub const ONE: Self = Self(CircuitBase::ONE);
}

impl From<u64> for CircuitBaseField {
    fn from(value: u64) -> Self {
        Self(CircuitBase::from(value))
    }
}

impl From<CircuitBase> for CircuitBaseField {
    fn from(value: CircuitBase) -> Self {
        Self(value)
    }
}

impl From<CircuitBaseField> for CircuitBase {
    fn from(value: CircuitBaseField) -> Self {
        value.0
    }
}

impl From<BaseFieldElement> for CircuitBaseField {
    fn from(value: BaseFieldElement) -> Self {
        value.0.into()
    }
}

impl From<CircuitBaseField> for BaseFieldElement {
    fn from(value: CircuitBaseField) -> Self {
        Self(value.into())
    }
}

impl From<BaseFieldElement> for CircuitBase {
    fn from(value: BaseFieldElement) -> Self {
        CircuitBaseField::from(value).into()
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

/// Merkle-tree leaf material used by Halo2 witness construction.
///
/// The first field stores the signer's verification key, and the second
/// field stores the lottery target value associated with that signer.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MTLeaf(pub SchnorrVerificationKey, pub Target);

/// Position of a sibling node relative to the current hash in a Merkle path.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Position {
    Left,
    Right,
}

impl From<Position> for CircuitBase {
    fn from(position: Position) -> Self {
        match position {
            Position::Left => Self::ZERO,
            Position::Right => Self::ONE,
        }
    }
}

impl From<Position> for CircuitBaseField {
    fn from(position: Position) -> Self {
        Self(CircuitBase::from(position))
    }
}

/// Merkle authentication path used by the Halo2 circuit witness.
///
/// Each entry stores sibling position and sibling hash value for one tree level.
#[derive(Clone, Debug, PartialEq, Eq)]
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
