//! Negative transition tests: tampered public inputs (fast CI) and MockProver
//! constraint checks (in `mod slow`), grouped by transition type.

use ff::Field;
use midnight_circuits::types::Instantiable;

use crate::StmResult;
use crate::circuits::halo2_ivc::{
    AssignedAccumulator, F,
    state::State,
    tests::common::{
        asset_readers::{RecursiveStepOutputAsset, load_embedded_verification_context_asset},
        helpers::verify_and_prepare_blake2b_recursive_proof,
    },
};

mod genesis;
mod next_epoch;
mod same_epoch;

/// Loads `verification_context` and a step output via `load_step_output`, tampers
/// `next_state` via `tamper`, then asserts the verifier rejects the result.
fn assert_step_output_rejects_tampered_state(
    load_step_output: impl FnOnce() -> StmResult<RecursiveStepOutputAsset>,
    load_label: &str,
    tamper: impl FnOnce(&mut State),
    rejection_message: &str,
) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let step_output =
        load_step_output().unwrap_or_else(|_| panic!("{load_label} asset should load"));

    let mut tampered_state = step_output.next_state.clone();
    tamper(&mut tampered_state);

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "{rejection_message}"
    );
}
