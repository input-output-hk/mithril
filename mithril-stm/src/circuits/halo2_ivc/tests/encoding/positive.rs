use ff::Field;
use midnight_proofs::utils::SerdeFormat;

use crate::circuits::halo2_ivc::{
    E, F, KZGCommitmentScheme, PREIMAGE_SIZE, S, VerifyingKey,
    circuit::IvcCircuit,
    io::{Read as IvcRead, Write as IvcWrite},
    state::State,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_verification_context_asset,
        },
        generators::{build_asset_generation_setup, build_genesis_protocol_message_preimage},
    },
};

#[test]
fn preimage_length_is_190_bytes() {
    // Off-circuit check that the genesis protocol message serializer produces
    // exactly PREIMAGE_SIZE bytes, matching the fixed byte ranges the circuit
    // reads for next_merkle_root, next_protocol_params, and current_epoch.
    let setup = build_asset_generation_setup();
    let preimage = build_genesis_protocol_message_preimage(&setup);
    assert_eq!(preimage.len(), PREIMAGE_SIZE);
}

#[test]
fn state_public_input_has_7_elements_in_correct_order() {
    // Off-circuit check that State::as_public_input() returns exactly 7 field
    // elements in the order the circuit expects: counter, msg, merkle_root,
    // next_merkle_root, protocol_params, next_protocol_params, current_epoch.
    let counter = F::from(1u64);
    let msg = F::from(2u64);
    let merkle_root = F::from(3u64);
    let next_merkle_root = F::from(4u64);
    let protocol_params = F::from(5u64);
    let next_protocol_params = F::from(6u64);
    let current_epoch = F::from(7u64);

    let state = State::new(
        counter,
        msg,
        merkle_root,
        next_merkle_root,
        protocol_params,
        next_protocol_params,
        current_epoch,
    );
    let public_input = state.as_public_input();

    assert_eq!(public_input.len(), 7);
    assert_eq!(public_input[0], counter);
    assert_eq!(public_input[1], msg);
    assert_eq!(public_input[2], merkle_root);
    assert_eq!(public_input[3], next_merkle_root);
    assert_eq!(public_input[4], protocol_params);
    assert_eq!(public_input[5], next_protocol_params);
    assert_eq!(public_input[6], current_epoch);
}

#[test]
fn genesis_state_all_fields_are_zero() {
    // Off-circuit check that State::genesis() initialises every field to zero,
    // matching the base-case contract the circuit relies on when no previous
    // recursive proof exists.
    let state = State::genesis();
    let public_input = state.as_public_input();

    assert!(
        public_input.iter().all(|f| f.is_zero().into()),
        "all fields of the genesis state should be zero"
    );
}

#[test]
fn accumulator_serialization_round_trip() {
    // Off-circuit check that the stored accumulator survives a write/read
    // round-trip with identical bytes, confirming the serialization format is
    // stable and lossless.
    let chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let mut first_bytes = Vec::new();
    chain_state
        .accumulator
        .write(&mut first_bytes, SerdeFormat::RawBytesUnchecked)
        .expect("accumulator serialization should succeed");

    let deserialized = crate::circuits::halo2_ivc::Accumulator::<S>::read(
        &mut first_bytes.as_slice(),
        SerdeFormat::RawBytesUnchecked,
    )
    .expect("accumulator deserialization should succeed");

    let mut second_bytes = Vec::new();
    deserialized
        .write(&mut second_bytes, SerdeFormat::RawBytesUnchecked)
        .expect("accumulator re-serialization should succeed");

    assert_eq!(
        first_bytes, second_bytes,
        "accumulator bytes should be identical after a round-trip"
    );
}

#[test]
fn vk_serialization_round_trip() {
    // Off-circuit check that the recursive verifying key survives a write/read
    // round-trip with an identical transcript_repr, confirming the VK
    // serialization format is stable and lossless.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");

    let original_repr = verification_context.recursive_verifying_key.transcript_repr();

    let mut bytes = Vec::new();
    verification_context
        .recursive_verifying_key
        .write(&mut bytes, SerdeFormat::RawBytesUnchecked)
        .expect("verifying key serialization should succeed");

    let deserialized = VerifyingKey::<F, KZGCommitmentScheme<E>>::read::<_, IvcCircuit>(
        &mut bytes.as_slice(),
        SerdeFormat::RawBytesUnchecked,
        (),
    )
    .expect("verifying key deserialization should succeed");

    assert_eq!(
        original_repr,
        deserialized.transcript_repr(),
        "verifying key transcript_repr should be identical after a round-trip"
    );
}
