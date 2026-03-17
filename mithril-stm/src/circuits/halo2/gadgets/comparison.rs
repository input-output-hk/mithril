use ff::{Field, PrimeField};
use midnight_circuits::instructions::{
    ArithInstructions, AssertionInstructions, AssignmentInstructions, BinaryInstructions,
    DecompositionInstructions, EqualityInstructions,
};
use midnight_circuits::types::{AssignedBit, AssignedNative};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::circuits::halo2::errors::to_synthesis_error;
use crate::circuits::halo2::gadgets::comparison_helpers::split_field_element_into_le_limbs;
use crate::circuits::halo2::types::CircuitBase;

/// Constrains two assigned field elements to share the same least-significant-bit parity.
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

/// Decomposes a 255-bit assigned value into `(low_127_bits, high_128_bits)` and constrains
/// the reconstruction to match the original witness.
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

    // Verify the least significant bit is consistent to make sure the decomposition is unique.
    assert_equal_parity(std_lib, layouter, x, &x_low_assigned)?;

    Ok((x_low_assigned, x_high_assigned))
}

/// Compares two assigned 255-bit values and returns a bit witnessing whether `x < y`.
pub(super) fn lower_than_native(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    x: &AssignedNative<CircuitBase>,
    y: &AssignedNative<CircuitBase>,
) -> Result<AssignedBit<CircuitBase>, Error> {
    let (x_low_assigned, x_high_assigned) = decompose_unsafe(std_lib, layouter, x)?;
    let (y_low_assigned, y_high_assigned) = decompose_unsafe(std_lib, layouter, y)?;

    let is_equal_high = std_lib.is_equal(layouter, &x_high_assigned, &y_high_assigned)?;
    let is_less_low = std_lib.lower_than(layouter, &x_low_assigned, &y_low_assigned, 127)?;
    let is_less_high = std_lib.lower_than(layouter, &x_high_assigned, &y_high_assigned, 128)?;

    let low_less = std_lib.and(layouter, &[is_equal_high, is_less_low])?;
    std_lib.or(layouter, &[is_less_high, low_less])
}
