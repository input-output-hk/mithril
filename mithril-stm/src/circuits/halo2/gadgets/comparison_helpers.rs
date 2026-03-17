use anyhow::anyhow;
use ff::PrimeField;
use num_bigint::BigUint;
use num_traits::{Num, One};

use crate::StmResult;
use crate::circuits::halo2::errors::StmCircuitError;

/// Splits a field element into `(lower, upper)` limbs at `num_bits` using LE encoding.
pub(super) fn split_field_element_into_le_limbs<Fp: PrimeField>(
    value: &Fp,
    num_bits: u32,
) -> StmResult<(Fp, Fp)> {
    let field_bits = Fp::NUM_BITS;
    if num_bits >= field_bits {
        return Err(anyhow!(StmCircuitError::InvalidBitDecompositionRange {
            num_bits,
            field_bits,
        }));
    }

    let value_big = BigUint::from_bytes_le(value.to_repr().as_ref());
    let lower_mask = (BigUint::one() << num_bits) - BigUint::one();
    let lower_big = value_big.clone() & &lower_mask;
    let upper_big = value_big >> num_bits;
    let lower = big_unsigned_integer_to_field_element::<Fp>(lower_big)?;
    let upper = big_unsigned_integer_to_field_element::<Fp>(upper_big)?;
    Ok((lower, upper))
}

/// Parses the prime-field modulus into a `BigUint` for limb splitting and reduction helpers.
fn field_modulus_as_biguint<Fp: PrimeField>() -> StmResult<BigUint> {
    BigUint::from_str_radix(&Fp::MODULUS[2..], 16)
        .map_err(|_| anyhow!(StmCircuitError::FieldModulusParseFailed))
}

/// Reduces a non-negative integer modulo the field modulus and converts it into a field element.
fn big_unsigned_integer_to_field_element<Fp: PrimeField>(e: BigUint) -> StmResult<Fp> {
    let modulus = field_modulus_as_biguint::<Fp>()?;
    let e = e % modulus;
    Fp::from_str_vartime(&e.to_str_radix(10)[..])
        .ok_or_else(|| anyhow!(StmCircuitError::FieldElementConversionFailed))
}
