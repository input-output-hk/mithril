use ff::PrimeField;
use num_bigint::BigUint;
use num_traits::{Num, One};
use thiserror::Error;

/// Errors returned by Halo2 utility conversion helpers.
#[derive(Debug, Error)]
pub(crate) enum Halo2UtilsError {
    /// Parsing the field modulus constant failed.
    #[error("failed to parse prime field modulus from hex")]
    FieldModulusParse,
    /// Converting a reduced integer to a field element failed.
    #[error("failed to convert reduced integer to field element")]
    FieldElementConversion,
}

/// Splits a field element into `(lower, upper)` limbs at `num_bits` using LE encoding.
///
/// The input is interpreted as a little-endian integer before bit slicing.
pub(crate) fn split_field_element_into_le_limbs<F: PrimeField>(
    value: &F,
    num_bits: u32,
) -> Result<(F, F), Halo2UtilsError> {
    let value_big = BigUint::from_bytes_le(value.to_repr().as_ref());
    let lower_mask = (BigUint::one() << num_bits) - BigUint::one();
    let lower_big = value_big.clone() & &lower_mask;
    let upper_big = value_big >> num_bits;
    let lower = big_unsigned_integer_to_field_element::<F>(lower_big)?;
    let upper = big_unsigned_integer_to_field_element::<F>(upper_big)?;
    Ok((lower, upper))
}

/// Parses the prime field modulus (`F::MODULUS`) into a `BigUint`.
fn field_modulus_as_biguint<F: PrimeField>() -> Result<BigUint, Halo2UtilsError> {
    BigUint::from_str_radix(&F::MODULUS[2..], 16).map_err(|_| Halo2UtilsError::FieldModulusParse)
}

/// Reduces an unsigned integer modulo `p` and converts it into the field element type.
///
/// Conversion uses `from_str_vartime` on the reduced decimal representation.
fn big_unsigned_integer_to_field_element<F: PrimeField>(e: BigUint) -> Result<F, Halo2UtilsError> {
    let modulus = field_modulus_as_biguint::<F>()?;
    let e = e % modulus;
    F::from_str_vartime(&e.to_str_radix(10)[..]).ok_or(Halo2UtilsError::FieldElementConversion)
}

#[cfg(test)]
pub(crate) use merkle_path_adapter::MerklePathAdapterError;

#[cfg(test)]
mod merkle_path_adapter {
    use crate::circuits::halo2::types::{MerklePath as Halo2MerklePath, Position};
    use crate::membership_commitment::MerklePath as StmMerklePath;
    use crate::signature_scheme::BaseFieldElement;
    use digest::Digest;
    use thiserror::Error;

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
                siblings.push((position, node));
            }

            Ok(Halo2MerklePath::new(siblings))
        }
    }
}
