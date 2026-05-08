//! Tests that `extract_fixed_bases` correctly reclassifies variable-basis MSM
//! terms into fixed-basis scalars, observable via changes in the public-input
//! encoding length and composition.
//!
//! A freshly converted `DualMSM → Accumulator` has all KZG polynomial-commitment
//! bases in its variable-base part. Calling `extract_fixed_bases` moves the N known
//! fixed bases to `rhs.fixed_base_scalars` (rhs = right-hand side of the pairing
//! equation `e(lhs, [τ]₂) = e(rhs, [1]₂)`), shrinking `as_public_input` by
//! `N * field_elements_per_base_point` (each extracted base loses its coordinate
//! encoding but keeps its scalar as a fixed-base entry).
//! The lhs (left-hand side) MSM is never touched by extraction.

use std::collections::BTreeMap;

use ff::Field;
use midnight_circuits::{types::Instantiable, verifier::AssignedMsm};

use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, C, S,
    state::trivial_acc,
    tests::common::{
        asset_readers::{
            load_embedded_recursive_chain_state_asset, load_embedded_verification_context_asset,
        },
        generators::build_recursive_fixed_bases,
        helpers::verify_prepare_poseidon_recursive_proof,
    },
};

/// Verifies the stored chain-state proof and returns the unextracted `DualMSM → Accumulator`
/// together with the recursive fixed-base map, ready for extraction tests.
///
/// The chain-state proof is a Poseidon recursive proof; its public inputs are
/// `[global | state | accumulator]` as stored in the committed assets.
fn build_unextracted_recursive_accumulator() -> (Accumulator<S>, BTreeMap<String, C>) {
    let verification_context =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let recursive_chain_state = load_embedded_recursive_chain_state_asset()
        .expect("recursive chain state asset should load");

    let public_inputs = [
        verification_context.global_field_elements.clone(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let dual_msm = verify_prepare_poseidon_recursive_proof(
        &verification_context.recursive_verifying_key,
        &recursive_chain_state.proof,
        &public_inputs,
    );

    let (_, recursive_fixed_bases, _) = build_recursive_fixed_bases(
        &verification_context.certificate_verifying_key,
        &verification_context.recursive_verifying_key,
    );

    (dual_msm.into(), recursive_fixed_bases)
}

#[test]
fn extract_fixed_bases_reduces_public_input_length_by_n_times_field_elements_per_base_point() {
    // Each extracted term removes one base point (field_elements_per_base_point field elements)
    // and one scalar from the variable-base part, then adds back only one scalar
    // as a fixed-base entry. Net reduction = field_elements_per_base_point per base.
    //
    // field_elements_per_base_point is derived from trivial_acc, which has exactly one base
    // and one scalar per side (lhs and rhs, i.e. left-hand side and right-hand side)
    // with no fixed-base entries:
    //   trivial_acc(&[]).len() == 2 * (field_elements_per_base_point + 1)
    let (mut accumulator, recursive_fixed_bases) = build_unextracted_recursive_accumulator();

    let fixed_base_count = recursive_fixed_bases.len();
    let trivial_accumulator_encoding_length =
        AssignedAccumulator::as_public_input(&trivial_acc(&[])).len();
    let field_elements_per_base_point = trivial_accumulator_encoding_length / 2 - 1;

    let encoding_length_before_extraction =
        AssignedAccumulator::as_public_input(&accumulator).len();
    accumulator.extract_fixed_bases(&recursive_fixed_bases);
    let encoding_length_after_extraction =
        AssignedAccumulator::as_public_input(&accumulator).len();

    assert_eq!(
        encoding_length_before_extraction - encoding_length_after_extraction,
        fixed_base_count * field_elements_per_base_point,
        "extract_fixed_bases must reduce as_public_input length by field_elements_per_base_point per extracted base",
    );
}

#[test]
fn extracted_fixed_base_scalars_are_non_trivial() {
    // The scalars moved into the fixed-base slot are KZG challenge-derived values;
    // at least one must be non-zero, confirming real values were reclassified.
    let (mut accumulator, recursive_fixed_bases) = build_unextracted_recursive_accumulator();

    accumulator.extract_fixed_bases(&recursive_fixed_bases);

    let public_input_encoding = AssignedAccumulator::as_public_input(&accumulator);
    let fixed_base_count = recursive_fixed_bases.len();

    // The last fixed_base_count entries of as_public_input are rhs.fixed_base_scalars
    // (rhs = right-hand side of the pairing equation, in BTreeMap order).
    assert!(
        public_input_encoding
            .iter()
            .rev()
            .take(fixed_base_count)
            .any(|f| !f.is_zero_vartime()),
        "at least one extracted fixed-base scalar must be non-zero",
    );
}

#[test]
fn extract_fixed_bases_does_not_affect_lhs() {
    // extract_fixed_bases only touches rhs (right-hand side); the lhs (left-hand side)
    // MSM encoding must be identical before and after the call.
    let (mut accumulator, recursive_fixed_bases) = build_unextracted_recursive_accumulator();

    let lhs_encoding_before_extraction =
        AssignedMsm::<S>::as_public_input(&accumulator.lhs());
    accumulator.extract_fixed_bases(&recursive_fixed_bases);
    let lhs_encoding_after_extraction =
        AssignedMsm::<S>::as_public_input(&accumulator.lhs());

    assert_eq!(
        lhs_encoding_before_extraction,
        lhs_encoding_after_extraction,
        "lhs MSM encoding must be unchanged by extract_fixed_bases",
    );
}

#[test]
#[should_panic]
fn extracting_same_fixed_bases_twice_panics() {
    // Attempting to extract the same fixed bases a second time violates the
    // contract (names already present in fixed_base_scalars) and must panic.
    // This test documents the current behaviour; it will be updated when
    // extract_fixed_bases is changed to return a Result.
    let (mut accumulator, recursive_fixed_bases) = build_unextracted_recursive_accumulator();
    accumulator.extract_fixed_bases(&recursive_fixed_bases);
    accumulator.extract_fixed_bases(&recursive_fixed_bases);
}
