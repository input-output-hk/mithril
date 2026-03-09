//! Test-only adapters for converting STM Merkle paths to Halo2 witness paths.

pub(crate) mod merkle_path_test_adapter {
    use digest::Digest;
    use thiserror::Error;

    use crate::circuits::halo2::types::{MerklePath as Halo2MerklePath, Position};
    use crate::membership_commitment::MerklePath as StmMerklePath;
    use crate::signature_scheme::BaseFieldElement;

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
}
