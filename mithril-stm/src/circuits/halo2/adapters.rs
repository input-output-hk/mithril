//! Adapter helpers that bridge Halo2 circuit-local wrapper types and Midnight backend types.

use crate::circuits::halo2::backend_reexports::BackendJubjubBase;
use crate::circuits::halo2::types::{CircuitBaseField, Position};
use ff::Field;

/// Convert a circuit wrapper field value into the backend base field.
pub(crate) fn to_backend_base(value: CircuitBaseField) -> BackendJubjubBase {
    value.into()
}

/// Convert a Merkle sibling position into backend bit encoding (left=0, right=1).
pub(crate) fn position_to_backend(position: Position) -> BackendJubjubBase {
    match position {
        Position::Left => BackendJubjubBase::ZERO,
        Position::Right => BackendJubjubBase::ONE,
    }
}

#[cfg(test)]
pub(crate) mod field_test_adapters {
    use crate::circuits::halo2::backend_reexports::BackendJubjubBase;
    use crate::circuits::halo2::types::CircuitBaseField;
    use crate::signature_scheme::BaseFieldElement;

    /// Convert a backend base field value into the circuit wrapper field.
    pub(crate) fn from_backend_base(value: BackendJubjubBase) -> CircuitBaseField {
        value.into()
    }

    /// Convert a circuit wrapper field value into a STM base field element wrapper.
    pub(crate) fn to_stm_base_field_element(value: CircuitBaseField) -> BaseFieldElement {
        BaseFieldElement(value.into())
    }

    /// Convert a STM base field element wrapper into a circuit wrapper field value.
    pub(crate) fn from_stm_base_field_element(value: BaseFieldElement) -> CircuitBaseField {
        value.0.into()
    }
}

#[cfg(test)]
pub(crate) mod merkle_path_test_adapter {
    use crate::circuits::halo2::adapters::field_test_adapters::from_backend_base;
    use crate::circuits::halo2::types::{MerklePath as Halo2MerklePath, Position};
    use crate::membership_commitment::MerklePath as StmMerklePath;
    use crate::signature_scheme::BaseFieldElement;
    use digest::Digest;
    use thiserror::Error;

    /// Errors returned when adapting STM Merkle paths to Halo2 witness paths.
    #[derive(Debug, Error)]
    pub enum MerklePathAdapterError {
        #[error("invalid merkle digest length")]
        InvalidDigestLength,
        #[error("non-canonical merkle digest")]
        NonCanonicalDigest,
    }

    impl<D: Digest> TryFrom<&StmMerklePath<D>> for Halo2MerklePath {
        type Error = MerklePathAdapterError;

        fn try_from(stm_path: &StmMerklePath<D>) -> Result<Self, Self::Error> {
            let mut siblings = Vec::with_capacity(stm_path.values.len());

            for (i, value) in stm_path.values.iter().enumerate() {
                let bytes: [u8; 32] = value
                    .as_slice()
                    .try_into()
                    .map_err(|_| MerklePathAdapterError::InvalidDigestLength)?;
                let node = BaseFieldElement::from_bytes(&bytes)
                    .ok()
                    .map(|base| base.0)
                    .ok_or(MerklePathAdapterError::NonCanonicalDigest)?;
                let bit = (stm_path.index >> i) & 1;
                // At level `i`, `bit = (index >> i) & 1`: `0` means current is left, `1` means right.
                // STM uses `H(current || sibling)` for `bit == 0`, else `H(sibling || current)`.
                // Map `0 -> Position::Right` and `1 -> Position::Left` so Halo2 folds identically.
                let position = if bit == 0 {
                    Position::Right
                } else {
                    Position::Left
                };
                siblings.push((position, from_backend_base(node)));
            }

            Ok(Halo2MerklePath::new(siblings))
        }
    }
}
