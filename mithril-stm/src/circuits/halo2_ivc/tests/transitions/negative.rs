use ff::Field;
use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator, F,
    tests::common::{
        asset_readers::{
            load_embedded_genesis_step_output_asset,
            load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        helpers::verify_and_prepare_blake2b_recursive_proof,
    },
};

#[test]
fn genesis_merkle_root_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // merkle_root is replaced in the public inputs, confirming the circuit
    // enforces merkle_root = 0 at the genesis base case.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered merkle_root should be rejected by the verifier"
    );
}

#[test]
fn genesis_protocol_params_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // protocol_params is replaced in the public inputs, confirming the circuit
    // enforces protocol_params = 0 at the genesis base case.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered protocol_params should be rejected by the verifier"
    );
}

#[test]
fn genesis_next_merkle_root_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // next_merkle_root is replaced in the public inputs, confirming the circuit
    // enforces next_merkle_root is extracted from the genesis message preimage.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.next_merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_merkle_root should be rejected by the verifier"
    );
}

#[test]
fn genesis_next_protocol_params_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // next_protocol_params is replaced in the public inputs, confirming the
    // circuit enforces next_protocol_params is extracted from the genesis
    // message preimage.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.next_protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_protocol_params should be rejected by the verifier"
    );
}

#[test]
fn genesis_counter_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // counter is replaced in the public inputs, confirming the circuit enforces
    // the counter transitions from 0 to 1 at the genesis base case.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.counter = F::from(2u64);

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered counter should be rejected by the verifier"
    );
}

#[test]
fn genesis_current_epoch_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // current_epoch is replaced in the public inputs, confirming the circuit
    // enforces current_epoch is extracted from the genesis message preimage
    // bytes [182..190].
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.current_epoch = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered current_epoch should be rejected by the verifier"
    );
}

#[test]
fn genesis_msg_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored genesis proof when
    // msg is replaced in the public inputs, confirming the circuit enforces
    // msg equals the genesis message committed in the Global public inputs.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let mut tampered_state = genesis_step_output.next_state.clone();
    tampered_state.msg = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &genesis_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered msg should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_merkle_root_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when merkle_root is replaced in the public inputs, confirming the circuit
    // enforces merkle_root = prev.merkle_root in a same-epoch transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered merkle_root should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_next_merkle_root_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when next_merkle_root is replaced in the public inputs, confirming the
    // circuit enforces next_merkle_root is extracted from the certificate
    // message preimage bytes [69..101].
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.next_merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_merkle_root should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_protocol_params_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when protocol_params is replaced in the public inputs, confirming the
    // circuit enforces protocol_params = prev.protocol_params in a same-epoch
    // transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered protocol_params should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_next_protocol_params_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when next_protocol_params is replaced in the public inputs, confirming
    // the circuit enforces next_protocol_params is extracted from the
    // certificate message preimage bytes [137..169].
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.next_protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_protocol_params should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_current_epoch_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when current_epoch is replaced in the public inputs, confirming the
    // circuit enforces current_epoch is unchanged in a same-epoch transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.current_epoch = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered current_epoch should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_counter_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when counter is replaced in the public inputs, confirming the circuit
    // enforces the counter increments by 1 at every recursive step.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.counter = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered counter should be rejected by the verifier"
    );
}

#[test]
fn same_epoch_msg_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored same-epoch proof
    // when msg is replaced in the public inputs, confirming the circuit
    // enforces msg equals the certificate message hash verified in-circuit.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let same_epoch_step_output = load_embedded_same_epoch_step_output_asset()
        .expect("same-epoch step output asset should load");

    let mut tampered_state = same_epoch_step_output.next_state.clone();
    tampered_state.msg = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&same_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &same_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered msg should be rejected by the verifier"
    );
}
