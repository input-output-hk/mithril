use midnight_circuits::instructions::{AssertionInstructions, EccInstructions};
use midnight_circuits::types::{AssignedNative, AssignedNativePoint, AssignedScalarOfNativeCurve};
use midnight_proofs::circuit::Layouter;
use midnight_proofs::plonk::Error;
use midnight_zk_stdlib::ZkStdLib;

use crate::circuits::halo2::types::{CircuitBase, CircuitCurve};

pub(crate) struct UniqueSchnorrSignatureInputs<'a> {
    pub(crate) dst_signature: &'a AssignedNative<CircuitBase>,
    pub(crate) generator: &'a AssignedNativePoint<CircuitCurve>,
    pub(crate) verification_key: &'a AssignedNativePoint<CircuitCurve>,
    pub(crate) response: &'a AssignedScalarOfNativeCurve<CircuitCurve>,
    pub(crate) challenge_in_base_field: &'a AssignedNative<CircuitBase>,
    pub(crate) challenge_as_scalar: &'a AssignedScalarOfNativeCurve<CircuitCurve>,
    pub(crate) hash: &'a AssignedNativePoint<CircuitCurve>,
    pub(crate) commitment_point: &'a AssignedNativePoint<CircuitCurve>,
}

pub(crate) fn verify_unique_signature(
    std_lib: &ZkStdLib,
    layouter: &mut impl Layouter<CircuitBase>,
    inputs: UniqueSchnorrSignatureInputs<'_>,
) -> Result<(), Error> {
    let cap_r_1 = std_lib.jubjub().msm(
        layouter,
        &[inputs.response.clone(), inputs.challenge_as_scalar.clone()],
        &[inputs.hash.clone(), inputs.commitment_point.clone()],
    )?;

    let cap_r_2 = std_lib.jubjub().msm(
        layouter,
        &[inputs.response.clone(), inputs.challenge_as_scalar.clone()],
        &[inputs.generator.clone(), inputs.verification_key.clone()],
    )?;

    let hx = std_lib.jubjub().x_coordinate(inputs.hash);
    let hy = std_lib.jubjub().y_coordinate(inputs.hash);
    let verification_key_x = std_lib.jubjub().x_coordinate(inputs.verification_key);
    let verification_key_y = std_lib.jubjub().y_coordinate(inputs.verification_key);
    let commitment_point_x = std_lib.jubjub().x_coordinate(inputs.commitment_point);
    let commitment_point_y = std_lib.jubjub().y_coordinate(inputs.commitment_point);
    let cap_r_1_x = std_lib.jubjub().x_coordinate(&cap_r_1);
    let cap_r_1_y = std_lib.jubjub().y_coordinate(&cap_r_1);
    let cap_r_2_x = std_lib.jubjub().x_coordinate(&cap_r_2);
    let cap_r_2_y = std_lib.jubjub().y_coordinate(&cap_r_2);

    let challenge_prime = std_lib.poseidon(
        layouter,
        &[
            inputs.dst_signature.clone(),
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

    std_lib.assert_equal(layouter, inputs.challenge_in_base_field, &challenge_prime)
}
