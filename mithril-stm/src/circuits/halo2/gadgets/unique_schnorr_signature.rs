use midnight_circuits::instructions::{AssertionInstructions, EccInstructions};
use midnight_circuits::types::{
    AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve,
};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};

#[allow(clippy::too_many_arguments)]
pub(crate) fn verify_unique_signature(
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
    let cap_r_1 = std_lib.jubjub().msm(
        layouter,
        &[response.clone(), challenge_scalar.clone()],
        &[hash.clone(), commitment_point.clone()],
    )?;

    let cap_r_2 = std_lib.jubjub().msm(
        layouter,
        &[response.clone(), challenge_scalar.clone()],
        &[generator.clone(), verification_key.clone()],
    )?;

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
