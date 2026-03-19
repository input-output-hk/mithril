//! Core Halo2 circuit primitives and reusable field/curve wrappers for STM.

use std::ops::{Add, AddAssign, Neg, Sub};

use ff::Field;
use midnight_curves::{Fq as MidnightBaseField, JubjubExtended as MidnightJubjub};

use crate::signature_scheme::BaseFieldElement;

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
