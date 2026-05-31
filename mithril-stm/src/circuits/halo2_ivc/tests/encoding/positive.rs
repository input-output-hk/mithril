//! Positive encoding tests: preimage layout, state public input format, and
//! serialization round-trips.

use ff::Field;
use midnight_proofs::utils::SerdeFormat;
use sha2::{Digest as Sha2Digest, Sha256};

use crate::MithrilMembershipDigest;
use crate::circuits::halo2_ivc::{
    Accumulator, E, F, KZGCommitmentScheme, PREIMAGE_CURRENT_EPOCH_BYTES,
    PREIMAGE_NEXT_MERKLE_ROOT_BYTES, PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES, PREIMAGE_SIZE, S,
    VerifyingKey,
    circuit::IvcCircuit,
    io::{Read as IvcRead, Write as IvcWrite},
    protocol_message::{ProtocolMessage, ProtocolMessagePartKey},
    state::State,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
            load_embedded_verification_context_asset,
        },
        generators::{build_asset_generation_setup, build_genesis_protocol_message_preimage},
    },
    types::{EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolParametersHash, StepCounter},
};

/// Minimal fixture for rigid preimage layout pinning tests.
/// Input: 40-byte legacy AVK (root_LE_32 || stake_BE_8); output: expected 44-byte slot.
fn build_test_message() -> (ProtocolMessage, [u8; 44]) {
    // Legacy AVK input: root(32) || total_stake_BE(8) — no leaf-count field.
    // from_bytes_legacy: root = bytes[0..32], stake = be_u64(bytes[32..40]).
    let mut avk_input = [0u8; 40];
    avk_input[39] = 1; // total_stake = 1, big-endian u64

    // to_rigid_slot_bytes output: root(32) || zeros(4) || stake_LE(8).
    let mut avk_slot = [0u8; 44];
    avk_slot[36] = 1; // total_stake = 1, little-endian u64

    let mut msg = ProtocolMessage::new();
    msg.set_message_part(
        ProtocolMessagePartKey::SnapshotDigest,
        hex::encode([2u8; 32]),
    );
    msg.set_message_part(
        ProtocolMessagePartKey::NextSnarkAggregateVerificationKey,
        hex::encode(avk_input),
    );
    msg.set_message_part(
        ProtocolMessagePartKey::NextProtocolParameters,
        hex::encode([7u8; 32]),
    );
    msg.set_message_part(ProtocolMessagePartKey::CurrentEpoch, "42".to_string());
    (msg, avk_slot)
}

#[test]
fn folded_accumulator_serialized_bytes_are_stable() {
    // Regression anchor: the serialized bytes of a folded accumulator must be
    // byte-for-byte identical to the committed golden file. This catches both
    // length-changing format shifts (different fixed-base count, encoding width,
    // length-prefix logic) and length-preserving ones (field reordering,
    // same-width field swap) that a round-trip test cannot detect.
    let recursive_step_output = load_embedded_recursive_step_output_asset()
        .expect("recursive step output asset should load");

    let mut bytes = Vec::new();
    recursive_step_output
        .next_accumulator
        .write(&mut bytes, SerdeFormat::RawBytesUnchecked)
        .expect("accumulator serialization should succeed");

    let expected_bytes =
        include_bytes!("../assets/recursive_step_output_accumulator_bytes.bin").as_slice();

    assert_eq!(
        bytes.as_slice(),
        expected_bytes,
        "serialized accumulator bytes should be identical to the committed golden file"
    );
}

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
        StepCounter::from_field(counter),
        MessageHash::from_field(msg),
        MerkleTreeCommitment::from_field(merkle_root),
        MerkleTreeCommitment::from_field(next_merkle_root),
        ProtocolParametersHash::from_field(protocol_params),
        ProtocolParametersHash::from_field(next_protocol_params),
        EpochNumber::from_field(current_epoch),
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

    let deserialized =
        Accumulator::<S>::read(&mut first_bytes.as_slice(), SerdeFormat::RawBytesUnchecked)
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
    //
    // transcript_repr is compared rather than raw bytes because the VK
    // serialization is not required to be byte-stable across invocations;
    // transcript_repr is the canonical identity used by the proof system.
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

// --- Rigid preimage layout pinning tests ---
// These pin each label and slot to its exact byte offset in the 190-byte rigid preimage,
// matching the layout the IVC circuit reads at PREIMAGE_NEXT_MERKLE_ROOT_BYTES,
// PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES, and PREIMAGE_CURRENT_EPOCH_BYTES.

#[test]
fn rigid_preimage_length_is_190_bytes() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed for a valid message");
    assert_eq!(preimage.len(), PREIMAGE_SIZE);
}

#[test]
fn rigid_preimage_digest_label_is_at_offset_0() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(&preimage[0..6], b"digest");
}

#[test]
fn rigid_preimage_dynamic_hash_is_at_offset_6() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    // Dynamic parts: only SnapshotDigest is non-fixed; SHA256("snapshot_digest" || hex([2u8;32]))
    let mut hasher = Sha256::new();
    hasher.update(b"snapshot_digest");
    hasher.update(hex::encode([2u8; 32]).as_bytes());
    let expected: [u8; 32] = hasher.finalize().into();
    assert_eq!(&preimage[6..38], &expected);
}

#[test]
fn rigid_preimage_avk_label_is_at_offset_38() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(&preimage[38..69], b"next_aggregate_verification_key");
}

#[test]
fn rigid_preimage_avk_slot_matches_expected_output() {
    let (msg, avk_slot) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(PREIMAGE_NEXT_MERKLE_ROOT_BYTES, 69..101);
    // AVK slot occupies 69..113: root(32) || zeros(4) || stake_LE(8).
    assert_eq!(&preimage[69..113], &avk_slot);
}

#[test]
fn rigid_preimage_params_label_is_at_offset_113() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(&preimage[113..137], b"next_protocol_parameters");
}

#[test]
fn rigid_preimage_params_slot_matches_input() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(PREIMAGE_NEXT_PROTOCOL_PARAMS_BYTES, 137..169);
    assert_eq!(&preimage[137..169], &[7u8; 32]);
}

#[test]
fn rigid_preimage_epoch_label_is_at_offset_169() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(&preimage[169..182], b"current_epoch");
}

#[test]
fn rigid_preimage_epoch_slot_is_42_le() {
    let (msg, _) = build_test_message();
    let preimage = msg
        .try_rigid_preimage::<MithrilMembershipDigest>()
        .expect("try_rigid_preimage should succeed");
    assert_eq!(PREIMAGE_CURRENT_EPOCH_BYTES, 182..190);
    assert_eq!(&preimage[182..190], &42u64.to_le_bytes());
}
