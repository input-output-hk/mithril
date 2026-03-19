//! Adapters from STM-side structures into the Halo2 circuit witness contract.
//!
//! Integrators should treat `witness.rs` as the circuit-facing boundary and use this module when
//! STM data needs explicit conversion into that shape.

use digest::Digest;
use thiserror::Error;

use crate::circuits::halo2::witness::{MerklePath as Halo2MerklePath, Position};
use crate::membership_commitment::MerklePath as StmMerklePath;
use crate::signature_scheme::BaseFieldElement;

/// Errors returned when adapting STM Merkle paths to Halo2 witness paths.
#[derive(Debug, Error)]
pub enum MerklePathAdapterError {
    /// A Merkle digest did not contain the expected 32-byte field encoding.
    #[error("invalid merkle digest length")]
    InvalidDigestLength,
    /// A 32-byte digest was not a canonical circuit base-field element encoding.
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
                .map(|base| base.into())
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
            siblings.push((position, node));
        }

        Ok(Halo2MerklePath::new(siblings))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::circuits::halo2::witness::Position;
    use crate::hash::poseidon::MidnightPoseidonDigest;
    use crate::membership_commitment::MerklePath as StmMerklePath;
    use crate::signature_scheme::BaseFieldElement;

    #[test]
    fn converts_valid_merkle_path_and_preserves_position_mapping() {
        let left_node = BaseFieldElement::from(5u64).to_bytes().to_vec();
        let right_node = BaseFieldElement::from(9u64).to_bytes().to_vec();
        let stm_path = StmMerklePath::<MidnightPoseidonDigest>::new(vec![left_node, right_node], 1);

        let halo2_path = Halo2MerklePath::try_from(&stm_path)
            .expect("valid STM merkle path should convert successfully");

        assert_eq!(halo2_path.siblings.len(), 2);
        assert_eq!(halo2_path.siblings[0].0, Position::Left);
        assert_eq!(halo2_path.siblings[1].0, Position::Right);
        assert_eq!(halo2_path.siblings[0].1, 5u64.into());
        assert_eq!(halo2_path.siblings[1].1, 9u64.into());
    }

    #[test]
    fn rejects_invalid_digest_length() {
        let stm_path = StmMerklePath::<MidnightPoseidonDigest>::new(vec![vec![0u8; 31]], 0);

        let error = Halo2MerklePath::try_from(&stm_path)
            .expect_err("invalid digest length should be rejected");

        assert!(matches!(error, MerklePathAdapterError::InvalidDigestLength));
    }

    #[test]
    fn rejects_non_canonical_digest() {
        let stm_path = StmMerklePath::<MidnightPoseidonDigest>::new(vec![vec![0xff; 32]], 0);

        let error = Halo2MerklePath::try_from(&stm_path)
            .expect_err("non-canonical digest should be rejected");

        assert!(matches!(error, MerklePathAdapterError::NonCanonicalDigest));
    }
}
