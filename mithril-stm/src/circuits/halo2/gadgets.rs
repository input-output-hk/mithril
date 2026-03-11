use anyhow::anyhow;
use ff::{Field, PrimeField};
use midnight_circuits::instructions::{
    ArithInstructions, AssertionInstructions, AssignmentInstructions, BinaryInstructions,
    ControlFlowInstructions, DecompositionInstructions, EccInstructions, EqualityInstructions,
};
use midnight_circuits::types::{
    AssignedBit, AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;
use num_bigint::BigUint;
use num_traits::{Num, One};

use crate::StmResult;
use crate::circuits::halo2::errors::{StmCircuitError, to_synthesis_error};
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};

/// Splits a field element into `(lower, upper)` limbs at `num_bits` using LE encoding.
fn split_field_element_into_le_limbs<Fp: PrimeField>(
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

fn field_modulus_as_biguint<Fp: PrimeField>() -> StmResult<BigUint> {
    BigUint::from_str_radix(&Fp::MODULUS[2..], 16)
        .map_err(|_| anyhow!(StmCircuitError::FieldModulusParseFailed))
}

fn big_unsigned_integer_to_field_element<Fp: PrimeField>(e: BigUint) -> StmResult<Fp> {
    let modulus = field_modulus_as_biguint::<Fp>()?;
    let e = e % modulus;
    Fp::from_str_vartime(&e.to_str_radix(10)[..])
        .ok_or_else(|| anyhow!(StmCircuitError::FieldElementConversionFailed))
}

fn assert_equal_parity(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    x: &AssignedNative<CircuitBase>,
    y: &AssignedNative<CircuitBase>,
) -> Result<(), Error> {
    let sgn0 = std_lib.sgn0(layouter, x)?;
    let sgn1 = std_lib.sgn0(layouter, y)?;
    std_lib.assert_equal(layouter, &sgn0, &sgn1)
}

// Decompose a 255-bit value into 127-bit and 128-bit values without checking the bound
fn decompose_unsafe(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    x: &AssignedNative<CircuitBase>,
) -> Result<(AssignedNative<CircuitBase>, AssignedNative<CircuitBase>), Error> {
    // Decompose 255-bit value into 127-bit and 128-bit values.
    let x_value = x.value();
    let base127 = CircuitBase::from_u128(1_u128 << 127);
    let (x_low, x_high) = x_value
        .map_with_result(|v| split_field_element_into_le_limbs(v, 127))
        .map_err(to_synthesis_error)?
        .unzip();

    let x_low_assigned: AssignedNative<_> = std_lib.assign(layouter, x_low)?;
    let x_high_assigned: AssignedNative<_> = std_lib.assign(layouter, x_high)?;

    let x_combined: AssignedNative<_> = std_lib.linear_combination(
        layouter,
        &[
            (CircuitBase::ONE, x_low_assigned.clone()),
            (base127, x_high_assigned.clone()),
        ],
        CircuitBase::ZERO,
    )?;
    std_lib.assert_equal(layouter, x, &x_combined)?;

    // Verify the least significant bit is consistent to make sure the decomposition is unique
    // This works because the modulus is an odd number
    assert_equal_parity(std_lib, layouter, x, &x_low_assigned)?;

    Ok((x_low_assigned, x_high_assigned))
}

// Compare x < y where x, y are 255-bit
fn lower_than_native(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    x: &AssignedNative<CircuitBase>,
    y: &AssignedNative<CircuitBase>,
) -> Result<AssignedBit<CircuitBase>, Error> {
    let (x_low_assigned, x_high_assigned) = decompose_unsafe(std_lib, layouter, x)?;
    let (y_low_assigned, y_high_assigned) = decompose_unsafe(std_lib, layouter, y)?;

    // Check if x < y
    let is_equal_high = std_lib.is_equal(layouter, &x_high_assigned, &y_high_assigned)?;
    let is_less_low = std_lib.lower_than(layouter, &x_low_assigned, &y_low_assigned, 127)?;
    let is_less_high = std_lib.lower_than(layouter, &x_high_assigned, &y_high_assigned, 128)?;

    let low_less = std_lib.and(layouter, &[is_equal_high, is_less_low])?;
    std_lib.or(layouter, &[is_less_high, low_less])
}

pub fn verify_merkle_path(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    verification_key: &AssignedNativePoint<CircuitCurve>,
    lottery_target_value: &AssignedNative<CircuitBase>,
    merkle_tree_commitment: &AssignedNative<CircuitBase>,
    merkle_siblings: &[AssignedNative<CircuitBase>],
    merkle_positions: &[AssignedBit<CircuitBase>],
) -> Result<(), Error> {
    let verification_key_x = std_lib.jubjub().x_coordinate(verification_key);
    let verification_key_y = std_lib.jubjub().y_coordinate(verification_key);
    let leaf = std_lib.poseidon(
        layouter,
        &[
            verification_key_x.clone(),
            verification_key_y.clone(),
            lottery_target_value.clone(),
        ],
    )?;
    let root =
        merkle_siblings
            .iter()
            .zip(merkle_positions.iter())
            .try_fold(leaf, |acc, (x, pos)| {
                // Choose the left child for hashing:
                // If pos is 1 (sibling on right) choose the current node else the sibling.
                let left = std_lib.select(layouter, pos, &acc, x)?;

                // Choose the right child for hashing:
                // If pos is 1 (sibling on right) choose the sibling else the current node.
                let right = std_lib.select(layouter, pos, x, &acc)?;

                std_lib.poseidon(layouter, &[left, right])
            })?;

    std_lib.assert_equal(layouter, &root, merkle_tree_commitment)
}

#[allow(clippy::too_many_arguments)]
pub fn verify_unique_signature(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    dst_signature: &AssignedNative<CircuitBase>,
    generator: &AssignedNativePoint<CircuitCurve>,
    verification_key: &AssignedNativePoint<CircuitCurve>,
    response: &AssignedScalarOfNativeCurve<CircuitCurve>,
    challenge_scalar: &AssignedScalarOfNativeCurve<CircuitCurve>,
    challenge_base: &AssignedNative<CircuitBase>,
    hash: &AssignedNativePoint<CircuitCurve>,
    commitment_point: &AssignedNativePoint<CircuitCurve>,
) -> Result<(), Error> {
    // Compute R1
    let cap_r_1 = std_lib.jubjub().msm(
        layouter,
        &[response.clone(), challenge_scalar.clone()],
        &[hash.clone(), commitment_point.clone()],
    )?;

    // Compute R2
    let cap_r_2 = std_lib.jubjub().msm(
        layouter,
        &[response.clone(), challenge_scalar.clone()],
        &[generator.clone(), verification_key.clone()],
    )?;

    // Compute H2(g, H1(message), verification_key, commitment_point, R1, R2)
    let hx = std_lib.jubjub().x_coordinate(hash);
    let hy = std_lib.jubjub().y_coordinate(hash);
    let verification_key_x = std_lib.jubjub().x_coordinate(verification_key);
    let verification_key_y = std_lib.jubjub().y_coordinate(verification_key);
    let commitment_point_x = std_lib.jubjub().x_coordinate(commitment_point);
    let commitment_point_y = std_lib.jubjub().y_coordinate(commitment_point);
    let cap_r_1_x = std_lib.jubjub().x_coordinate(&cap_r_1);
    let cap_r_1_y = std_lib.jubjub().y_coordinate(&cap_r_1);
    let cap_r_2_x = std_lib.jubjub().x_coordinate(&cap_r_2);
    let cap_r_2_y = std_lib.jubjub().y_coordinate(&cap_r_2);

    let challenge_prime = std_lib.poseidon(
        layouter,
        &[
            dst_signature.clone(),
            hx,
            hy,
            verification_key_x,
            verification_key_y,
            commitment_point_x.clone(),
            commitment_point_y.clone(),
            cap_r_1_x,
            cap_r_1_y,
            cap_r_2_x,
            cap_r_2_y,
        ],
    )?;

    std_lib.assert_equal(layouter, challenge_base, &challenge_prime)
}

pub fn is_lottery_won(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    lottery_prefix: &AssignedNative<CircuitBase>,
    commitment_point: &AssignedNativePoint<CircuitCurve>,
    lottery_index: &AssignedNative<CircuitBase>,
    lottery_target_value: &AssignedNative<CircuitBase>,
) -> Result<(), Error> {
    let commitment_point_x = std_lib.jubjub().x_coordinate(commitment_point);
    let commitment_point_y = std_lib.jubjub().y_coordinate(commitment_point);
    let lottery_evaluation_value = std_lib.poseidon(
        layouter,
        &[
            lottery_prefix.clone(),
            commitment_point_x,
            commitment_point_y,
            lottery_index.clone(),
        ],
    )?;
    let is_less = lower_than_native(
        std_lib,
        layouter,
        lottery_target_value,
        &lottery_evaluation_value,
    )?;
    std_lib.assert_false(layouter, &is_less)
}
