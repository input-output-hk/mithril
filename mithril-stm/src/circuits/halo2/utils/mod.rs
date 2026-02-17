use ff::PrimeField;
use num_bigint::BigUint;
use num_traits::{Num, One};

/// Split a field element into lower/upper limbs at `num_bits` (little-endian).
pub(crate) fn split<F: PrimeField>(value: &F, num_bits: u32) -> (F, F) {
    let value_big = BigUint::from_bytes_le(value.to_repr().as_ref());
    let lower_mask = (BigUint::one() << num_bits) - BigUint::one();
    let lower_big = value_big.clone() & &lower_mask;
    let upper_big = value_big >> num_bits;
    let lower = big_to_fe::<F>(lower_big);
    let upper = big_to_fe::<F>(upper_big);
    (lower, upper)
}

fn modulus<F: PrimeField>() -> BigUint {
    BigUint::from_str_radix(&F::MODULUS[2..], 16).unwrap()
}

fn big_to_fe<F: PrimeField>(e: BigUint) -> F {
    let modulus = modulus::<F>();
    let e = e % modulus;
    F::from_str_vartime(&e.to_str_radix(10)[..]).unwrap()
}

#[cfg(test)]
use crate::circuits::halo2::types::{JubjubBase, MerklePath as Halo2MerklePath, Position};
#[cfg(test)]
use crate::membership_commitment::MerklePath as StmMerklePath;
#[cfg(test)]
use digest::Digest;
#[cfg(test)]
use thiserror::Error;

/// Strictly decode a 32-byte little-endian encoding into a Jubjub base field element.
/// Returns None on non-canonical encodings.
#[cfg(test)]
pub(crate) fn jubjub_base_from_le_bytes_strict(bytes: &[u8; 32]) -> Option<JubjubBase> {
    JubjubBase::from_bytes_le(bytes).into_option()
}

/// Convert digest bytes to a Jubjub base field element using strict decoding.
#[cfg(test)]
pub(crate) fn digest_bytes_to_base(bytes: &[u8; 32]) -> Option<JubjubBase> {
    jubjub_base_from_le_bytes_strict(bytes)
}

#[cfg(test)]
#[derive(Debug, Error)]
pub enum MerklePathAdapterError {
    #[error("invalid merkle digest length")]
    InvalidDigestLength,
    #[error("non-canonical merkle digest")]
    NonCanonicalDigest,
}

#[cfg(test)]
impl<D: Digest> TryFrom<&StmMerklePath<D>> for Halo2MerklePath {
    type Error = MerklePathAdapterError;

    fn try_from(stm_path: &StmMerklePath<D>) -> Result<Self, Self::Error> {
        let mut siblings = Vec::with_capacity(stm_path.values.len());

        for (i, value) in stm_path.values.iter().enumerate() {
            let bytes: [u8; 32] = value
                .as_slice()
                .try_into()
                .map_err(|_| MerklePathAdapterError::InvalidDigestLength)?;
            let node =
                digest_bytes_to_base(&bytes).ok_or(MerklePathAdapterError::NonCanonicalDigest)?;
            let bit = (stm_path.index >> i) & 1;
            // STM uses even idx => H(h || sibling), odd idx => H(sibling || h);
            // map even to Position::Right so Halo2 folds as H(acc || sibling).
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
