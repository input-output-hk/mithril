use ff::Field;
use midnight_circuits::types::Instantiable;

use crate::circuits::halo2_ivc::{
    AssignedAccumulator, F,
    circuit::IvcCircuit,
    state::{State, Witness},
    tests::common::{
        asset_readers::{
            load_embedded_genesis_step_output_asset,
            load_embedded_recursive_chain_state_asset,
            load_embedded_recursive_step_output_asset,
            load_embedded_same_epoch_step_output_asset,
            load_embedded_verification_context_asset,
        },
        generators::{
            GENESIS_EPOCH, build_asset_generation_setup, build_genesis_base_case_next_state,
            build_genesis_base_case_witness, next_message_and_preimage_for_step,
            next_state_for_step, same_epoch_message_and_preimage_for_step,
            same_epoch_next_state_for_step,
        },
        helpers::{
            assert_recursive_mock_prover_rejects, build_recursive_mock_prover_setup,
            verify_and_prepare_blake2b_recursive_proof,
        },
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

#[test]
fn next_epoch_merkle_root_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when merkle_root is replaced in the public inputs, confirming the circuit
    // enforces merkle_root = prev.next_merkle_root in a next-epoch transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered merkle_root should be rejected by the verifier"
    );
}

#[test]
fn next_epoch_protocol_params_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when protocol_params is replaced in the public inputs, confirming the
    // circuit enforces protocol_params = prev.next_protocol_params in a
    // next-epoch transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered protocol_params should be rejected by the verifier"
    );
}

#[test]
fn next_epoch_next_merkle_root_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when next_merkle_root is replaced in the public inputs.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.next_merkle_root = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_merkle_root should be rejected by the verifier"
    );
}

#[test]
fn next_epoch_next_protocol_params_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when next_protocol_params is replaced in the public inputs.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.next_protocol_params = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered next_protocol_params should be rejected by the verifier"
    );
}

#[test]
fn next_epoch_counter_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when counter is replaced in the public inputs, confirming the circuit
    // enforces counter = prev.counter + 1 in a next-epoch transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.counter = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered counter should be rejected by the verifier"
    );
}

#[test]
fn next_epoch_current_epoch_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when current_epoch is replaced in the public inputs, confirming the
    // circuit enforces current_epoch = prev.current_epoch + 1 in a next-epoch
    // transition.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.current_epoch = F::ZERO;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered current_epoch should be rejected by the verifier"
    );
}

#[test]
fn next_epoch_msg_tampered_is_rejected() {
    // Asset-based check that the verifier rejects a stored next-epoch proof
    // when msg is replaced in the public inputs.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let next_epoch_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut tampered_state = next_epoch_step_output.next_state.clone();
    tampered_state.msg = F::ONE;

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_epoch_step_output.next_accumulator),
    ]
    .concat();

    let dual_msm = verify_and_prepare_blake2b_recursive_proof(
        &verification_context.recursive_verifying_key,
        &next_epoch_step_output.proof,
        &public_inputs,
    );

    assert!(
        !dual_msm.check(&verification_context.verifier_params),
        "proof with tampered msg should be rejected by the verifier"
    );
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_same_epoch_with_wrong_merkle_root_linkage() {
    // MockProver check that the circuit rejects a same-epoch witness where the
    // public inputs claim merkle_root ≠ prev_state.merkle_root, confirming the
    // in-circuit constraint that carries the Merkle root unchanged across a
    // same-epoch transition is wired correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let prev_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load")
        .state;

    let (message, preimage_bytes) = same_epoch_message_and_preimage_for_step(&setup, &prev_state);
    let witness = Witness::new(
        setup.genesis_signature.clone(),
        prev_state.merkle_root,
        message,
        preimage_bytes
            .try_into()
            .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
    );

    let correct_next_state = same_epoch_next_state_for_step(&prev_state, message);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.merkle_root = F::ONE;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        prev_state,
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_same_epoch_with_wrong_protocol_params_linkage() {
    // MockProver check that the circuit rejects a same-epoch witness where the
    // public inputs claim protocol_params ≠ prev_state.protocol_params,
    // confirming the in-circuit constraint that carries protocol parameters
    // unchanged across a same-epoch transition is wired correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let prev_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load")
        .state;

    let (message, preimage_bytes) = same_epoch_message_and_preimage_for_step(&setup, &prev_state);
    let witness = Witness::new(
        setup.genesis_signature.clone(),
        prev_state.merkle_root,
        message,
        preimage_bytes
            .try_into()
            .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
    );

    let correct_next_state = same_epoch_next_state_for_step(&prev_state, message);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.protocol_params = F::ONE;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        prev_state,
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_next_epoch_with_wrong_protocol_params_linkage() {
    // MockProver check that the circuit rejects a next-epoch witness where the
    // public inputs claim protocol_params ≠ prev_state.next_protocol_params,
    // confirming the in-circuit constraint that advances protocol parameters to
    // the previous next_protocol_params across a next-epoch transition is wired
    // correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let prev_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load")
        .state;

    let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
    let witness = Witness::new(
        setup.genesis_signature.clone(),
        prev_state.next_merkle_root,
        message,
        preimage_bytes
            .try_into()
            .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
    );

    let correct_next_state = next_state_for_step(&prev_state, message);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.protocol_params = F::ONE;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        prev_state,
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_next_epoch_with_wrong_merkle_root_linkage() {
    // MockProver check that the circuit rejects a next-epoch witness where the
    // public inputs claim merkle_root ≠ prev_state.next_merkle_root, confirming
    // the in-circuit constraint that advances the Merkle root to the previous
    // next_merkle_root across a next-epoch transition is wired correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let prev_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load")
        .state;

    let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
    let witness = Witness::new(
        setup.genesis_signature.clone(),
        prev_state.next_merkle_root,
        message,
        preimage_bytes
            .try_into()
            .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
    );

    let correct_next_state = next_state_for_step(&prev_state, message);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.merkle_root = F::ONE;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        prev_state,
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_same_epoch_with_wrong_current_epoch_linkage() {
    // MockProver check that the circuit rejects a same-epoch witness where the
    // public inputs claim current_epoch ≠ prev_state.current_epoch, confirming
    // the in-circuit constraint that keeps the epoch unchanged across a
    // same-epoch transition is wired correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let prev_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load")
        .state;

    let (message, preimage_bytes) = same_epoch_message_and_preimage_for_step(&setup, &prev_state);
    let witness = Witness::new(
        setup.genesis_signature.clone(),
        prev_state.merkle_root,
        message,
        preimage_bytes
            .try_into()
            .expect("same-epoch preimage should be PREIMAGE_SIZE bytes"),
    );

    let correct_next_state = same_epoch_next_state_for_step(&prev_state, message);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.current_epoch = prev_state.current_epoch + F::ONE;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        prev_state,
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_next_epoch_with_wrong_current_epoch_increment() {
    // MockProver check that the circuit rejects a next-epoch witness where the
    // public inputs claim current_epoch = prev_state.current_epoch (no
    // increment), confirming the in-circuit constraint that advances the epoch
    // by exactly one across a next-epoch transition is wired correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let prev_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load")
        .state;

    let (message, preimage_bytes) = next_message_and_preimage_for_step(&setup, &prev_state);
    let witness = Witness::new(
        setup.genesis_signature.clone(),
        prev_state.next_merkle_root,
        message,
        preimage_bytes
            .try_into()
            .expect("next-epoch preimage should be PREIMAGE_SIZE bytes"),
    );

    let correct_next_state = next_state_for_step(&prev_state, message);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.current_epoch = prev_state.current_epoch;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        prev_state,
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}

// TODO: Move this slow transition test into a dedicated slow/extended CI mode once
// the recursive test suite is split into fast and slow lanes.
#[test]
fn slow_circuit_rejects_msg_inconsistent_with_preimage() {
    // MockProver check that the circuit rejects a genesis witness where msg in
    // the public inputs does not match Blake2b(msg_preimage), confirming the
    // in-circuit hash constraint between the preimage bytes and the resulting
    // msg field is wired correctly.
    let setup = build_asset_generation_setup();
    let mock_prover_setup = build_recursive_mock_prover_setup(&setup);

    let witness = build_genesis_base_case_witness(&setup);

    let correct_next_state = build_genesis_base_case_next_state(&setup, GENESIS_EPOCH);
    let mut tampered_state = correct_next_state.clone();
    tampered_state.msg = F::ONE;

    let circuit = IvcCircuit::new(
        mock_prover_setup.global.clone(),
        State::genesis(),
        witness,
        vec![],
        vec![],
        mock_prover_setup.trivial_accumulator.clone(),
        mock_prover_setup.certificate_verifying_key.vk(),
        &mock_prover_setup.recursive_verifying_key,
    );

    let public_inputs = [
        mock_prover_setup.global.as_public_input(),
        tampered_state.as_public_input(),
        AssignedAccumulator::as_public_input(&mock_prover_setup.trivial_accumulator),
    ]
    .concat();

    assert_recursive_mock_prover_rejects(circuit, public_inputs);
}
