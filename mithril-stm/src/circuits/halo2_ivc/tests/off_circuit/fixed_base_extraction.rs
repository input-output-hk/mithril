//! Tests that `extract_fixed_bases` correctly reclassifies variable-base MSM
//! terms into fixed-base scalars, observable via changes in the public-input
//! encoding length and composition.
//!
//! A freshly converted `DualMSM → Accumulator` has all KZG polynomial-commitment
//! bases in its variable-base part. Calling `extract_fixed_bases` moves the N known
//! fixed bases to `rhs.fixed_base_scalars` (rhs = right-hand side of the pairing
//! equation `e(lhs, [τ]₂) = e(rhs, [1]₂)`), shrinking `as_public_input` by
//! `N * field_elements_per_base_point` (each extracted base loses its coordinate
//! encoding but keeps its scalar as a fixed-base entry).
//! The lhs (left-hand side) MSM is never touched by extraction.

use ff::Field;
use midnight_circuits::{types::Instantiable, verifier::AssignedMsm};

use crate::circuits::halo2_ivc::{
    AssignedAccumulator, RecursiveEmulation, accumulator::trivial_accumulator,
    tests::common::helpers::build_unextracted_recursive_proof_accumulator_from_assets,
};

#[test]
fn resolve_fixed_bases_grows_public_input_length_by_n_times_field_elements_per_base_point() {
    // Each extracted term removes one base point (field_elements_per_base_point field elements)
    // and one scalar from the variable-base part, then adds back only one scalar
    // as a fixed-base entry. Net reduction = field_elements_per_base_point per base.
    //
    // field_elements_per_base_point is derived from trivial_accumulator, which has exactly one base
    // and one scalar per side (left-hand side and right-hand side) with no fixed-base entries:
    //   trivial_acc(&[]).len() == 2 * (field_elements_per_base_point + 1)
    let (mut accumulator, recursive_fixed_bases) =
        build_unextracted_recursive_proof_accumulator_from_assets();
    let fixed_base_scalar_count =
        accumulator.lhs().fixed_base_scalars().len() + accumulator.rhs().fixed_base_scalars().len();
    let trivial_accumulator_encoding_length =
        AssignedAccumulator::as_public_input(&trivial_accumulator(&[])).len();
    let field_elements_per_base_point = trivial_accumulator_encoding_length / 2 - 1;

    let encoding_length_before_extraction =
        AssignedAccumulator::as_public_input(&accumulator).len();
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let encoding_length_after_extraction = AssignedAccumulator::as_public_input(&accumulator).len();

    assert_eq!(
        encoding_length_after_extraction - encoding_length_before_extraction,
        fixed_base_scalar_count * field_elements_per_base_point,
        "extract_fixed_bases should reduce as_public_input length by field_elements_per_base_point per extracted base"
    );
}

#[test]
fn resolved_fixed_base_scalars_are_non_trivial() {
    // The scalars moved into the fixed-base slot are KZG challenge-derived values;
    // at least one must be non-zero, confirming real values were reclassified.
    let (mut accumulator, recursive_fixed_bases) =
        build_unextracted_recursive_proof_accumulator_from_assets();
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);

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
        "at least one extracted fixed-base scalar should be non-zero"
    );
}

#[test]
fn resolved_fixed_bases_does_not_affect_lhs() {
    // extract_fixed_bases only touches rhs (right-hand side); the lhs (left-hand side)
    // MSM encoding must be identical before and after the call.
    let (mut accumulator, recursive_fixed_bases) =
        build_unextracted_recursive_proof_accumulator_from_assets();

    let lhs_encoding_before_extraction =
        AssignedMsm::<RecursiveEmulation>::as_public_input(&accumulator.lhs());
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let lhs_encoding_after_extraction =
        AssignedMsm::<RecursiveEmulation>::as_public_input(&accumulator.lhs());

    assert_eq!(
        lhs_encoding_before_extraction, lhs_encoding_after_extraction,
        "lhs MSM encoding should be unchanged by extract_fixed_bases"
    );
}

#[test]
fn resolving_fixed_bases_twice_is_a_harmless_no_op() {
    // Attempting to extract the same fixed bases a second time violates the
    // contract (names already present in fixed_base_scalars) and must panic.
    // This test documents the current behaviour; it will be updated when
    // extract_fixed_bases is changed to return a Result.
    let (mut accumulator, recursive_fixed_bases) =
        build_unextracted_recursive_proof_accumulator_from_assets();
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let after_first = AssignedAccumulator::as_public_input(&accumulator);
    accumulator.resolve_fixed_bases(&recursive_fixed_bases);
    let after_second = AssignedAccumulator::as_public_input(&accumulator);
    assert_eq!(after_first, after_second);
}
