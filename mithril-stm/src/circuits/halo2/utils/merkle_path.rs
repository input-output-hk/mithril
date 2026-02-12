use digest::Digest;

use crate::circuits::halo2::off_circuit::merkle_tree::{MerklePath as Halo2MerklePath, Position};
use crate::circuits::halo2::types::JubjubBase;
use crate::circuits::halo2::utils::field_bytes::digest_bytes_to_base;
use crate::membership_commitment::MerklePath as StmMerklePath;

#[derive(Debug)]
pub(crate) enum MerklePathAdapterError {
    InvalidDigestLength,
    NonCanonicalDigest,
}

pub(crate) fn stm_path_to_halo2_path<D: Digest>(
    stm_path: &StmMerklePath<D>,
) -> Result<Halo2MerklePath, MerklePathAdapterError> {
    let mut siblings = Vec::with_capacity(stm_path.values.len());

    for (i, value) in stm_path.values.iter().enumerate() {
        let bytes: [u8; 32] = value
            .as_slice()
            .try_into()
            .map_err(|_| MerklePathAdapterError::InvalidDigestLength)?;
        let node = digest_bytes_to_base(&bytes).ok_or(MerklePathAdapterError::NonCanonicalDigest)?;
        let bit = (stm_path.index >> i) & 1;
        // STM uses even idx => H(h || sibling), odd idx => H(sibling || h);
        // map even to Position::Right so Halo2 folds as H(acc || sibling).
        let position = if bit == 0 { Position::Right } else { Position::Left };
        siblings.push((position, node));
    }

    Ok(Halo2MerklePath::new(siblings))
}
