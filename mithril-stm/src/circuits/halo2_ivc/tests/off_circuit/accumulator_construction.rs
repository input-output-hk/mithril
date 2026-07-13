//! Tests that `trivial_accumulator` has the expected structure, verifiable through
//! its public-input encoding.

use midnight_circuits::{types::Instantiable, verifier::Accumulator};

use crate::circuits::halo2_ivc::{
    AssignedAccumulator,
    accumulator::trivial_accumulator,
    midnight_backend::RecursiveEmulation,
    tests::common::{
        asset_readers::{
            load_embedded_genesis_step_output_asset, load_embedded_recursive_chain_state_asset,
            load_embedded_verification_context_asset,
        },
        generators::build_recursive_fixed_bases,
        helpers::verify_prepare_poseidon_recursive_proof,
    },
};

#[test]
fn trivial_acc_public_inputs_match_stored_genesis_accumulator() {
    // The genesis asset stores a trivial_accumulator clone as next_accumulator.
    // Rebuilding trivial_accumulator from the same fixed-base name set must produce
    // an identical public-input encoding.
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let genesis_step_output =
        load_embedded_genesis_step_output_asset().expect("genesis step output asset should load");

    let combined_fixed_base_names: Vec<String> =
        verification_context.combined_fixed_bases.keys().cloned().collect();

    let accumulator = trivial_accumulator(&combined_fixed_base_names);

    assert_eq!(
        AssignedAccumulator::as_public_input(&accumulator),
        AssignedAccumulator::as_public_input(&genesis_step_output.next_accumulator),
        "trivial_accumulator public inputs should match the stored genesis next_accumulator"
    );
}

#[test]
fn trivial_acc_public_input_length_scales_with_fixed_base_name_count() {
    // Each additional fixed-base name adds exactly one scalar to the rhs
    // fixed_base_scalars map, which contributes one field element to the
    // public-input encoding.
    let empty_accumulator_encoding_length =
        AssignedAccumulator::as_public_input(&trivial_accumulator(&[])).len();

    let three_fixed_base_names: Vec<String> =
        ["a", "b", "c"].iter().map(|name| name.to_string()).collect();
    let encoding_length_with_three_names =
        AssignedAccumulator::as_public_input(&trivial_accumulator(&three_fixed_base_names)).len();

    assert_eq!(
        encoding_length_with_three_names,
        empty_accumulator_encoding_length + 3,
        "each fixed-base name should add exactly one field element to the public-input encoding"
    );
}

#[test]
#[should_panic]
fn wrong_prefix_for_fixed_bases_fails_accumulator_creation() {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let (_, recursive_fixed_bases, _) = build_recursive_fixed_bases(
        &verification_context.certificate_verifying_key,
        &verification_context.recursive_verifying_key,
    );

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let _accumulator: Accumulator<RecursiveEmulation> = Accumulator::from_dual_msm(
        verify_prepare_poseidon_recursive_proof(
            verification_context.recursive_verifying_key.as_ref(),
            recursive_chain_state.ivc_proof.as_bytes(),
            &public_inputs,
        ),
        "wrong_prefix",
        &recursive_fixed_bases,
    );
}
