use midnight_circuits::instructions::EccInstructions;
use midnight_circuits::types::{AssignedNative, AssignedNativePoint};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::circuits::halo2::gadgets::comparison::lower_than_native;
use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};

pub(crate) fn assert_lottery_won(
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

pub(crate) fn assert_strictly_increasing_lottery_index(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    previous_lottery_index: &AssignedNative<CircuitBase>,
    lottery_index: &AssignedNative<CircuitBase>,
) -> Result<(), Error> {
    let is_less =
        std_lib.lower_than(layouter, previous_lottery_index, lottery_index, 32)?;
    std_lib.assert_true(layouter, &is_less)
}

pub(crate) fn assert_lottery_index_in_bounds(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    lottery_index: &AssignedNative<CircuitBase>,
    m: &AssignedNative<CircuitBase>,
) -> Result<(), Error> {
    let is_less = std_lib.lower_than(layouter, lottery_index, m, 32)?;
    std_lib.assert_true(layouter, &is_less)
}
