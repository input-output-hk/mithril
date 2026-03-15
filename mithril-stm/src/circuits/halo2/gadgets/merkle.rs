use midnight_circuits::instructions::{AssertionInstructions, ControlFlowInstructions, EccInstructions};
use midnight_circuits::types::{AssignedBit, AssignedNative, AssignedNativePoint};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};

pub(crate) fn verify_merkle_path(
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
                let left = std_lib.select(layouter, pos, &acc, x)?;
                let right = std_lib.select(layouter, pos, x, &acc)?;
                std_lib.poseidon(layouter, &[left, right])
            })?;

    std_lib.assert_equal(layouter, &root, merkle_tree_commitment)
}
